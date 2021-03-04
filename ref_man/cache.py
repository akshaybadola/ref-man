import os
import time
import shutil
from subprocess import Popen, PIPE, TimeoutExpired
from threading import Thread, Event


class CacheHelper:
    def __init__(self, local_dir, remote_dir, cache_file, logger):
        self.local_dir = local_dir
        self.remote_dir = remote_dir
        self.cache_file = cache_file
        self.updating_ev = Event()
        self.success_ev = Event()
        self.success_with_errors_ev = Event()
        self.update_thread = None
        self.logger = logger
        self.check_and_fix_cache()

    @property
    def updating(self):
        return self.updating_ev.is_set()

    @property
    def finished(self):
        return self.success_ev.is_set()

    @property
    def finished_with_errors(self):
        return self.success_with_errors_ev.is_set()

    # TODO: Change to sqlite
    def read_cache(self):
        local_files = [os.path.join(self.local_dir, f)
                       for f in os.listdir(self.local_dir)
                       if not f.startswith(".")]
        with open(self.cache_file) as f:
            cache = [x for x in f.read().split("\n") if len(x)]
            cached_files = [x.rsplit(";")[0] for x in cache]
        return local_files, cache, cached_files

    @property
    def cache_needs_updating(self):
        lf, cache, cf = self.read_cache()
        files = set(lf) - set(cf)
        return files

    def stop_update(self):
        self.updating_ev.clear()

    def shutdown(self):
        self.stop_update()
        if self.update_thread is not None:
            self.update_thread.join()

    def check_and_fix_cache(self):
        self.logger.debug("Checking existing cache")
        local_files, cache, remote_files = self.read_cache()
        broken_links = [c.split(";")[0] for c in cache if c.split(";")[1] == ""]
        self.logger.debug(f"Found {len(broken_links)} broken links. Updating")
        self.update_thread = Thread(target=self.update_cache_helper, args=[broken_links])
        self.update_thread.start()

    def copy_file(self):
        try:
            p = Popen(f"rclone --no-update-modtime -v copy {self.local_path} {self.remote_dir}",
                      shell=True, stdout=PIPE, stderr=PIPE)
            out, err = p.communicate(timeout=10)
            err = err.decode("utf-8").lower()
            if err and ("copied" in err or "transferred" in err):
                self.logger.debug(f"Copied file {self.local_path} to remote")
                status = True
            else:
                status = False
        except TimeoutExpired:
            self.logger.warning(f"Timeout while copying for file {self.local_path}")
            status = False
        return status

    def try_get_link(self, f, remote_path):
        try:
            p = Popen(f"rclone -v link {remote_path}", shell=True, stdout=PIPE, stderr=PIPE)
            out, err = p.communicate(timeout=10)
            if err and "error 403" in err.decode("utf-8").lower():
                status = False
                link = "not_present"
            else:
                link = out.decode("utf-8").replace("\n", "")
                status = True
        except TimeoutExpired:
            self.logger.warning(f"Timeout while getting link for file {f}")
            link = "timeout"
            status = False
        return status, link

    def get_link(self, f, cache, warnings):
        try:
            start = time.time()
            remote_path = os.path.join(self.remote_dir, os.path.basename(f))
            if " " in remote_path:
                remote_path = f'"{remote_path}"'
            status, link = self.try_get_link(f, remote_path)
            if not status:
                if link == "not_present":
                    self.logger.warning(f"File {f} does not exist on remote. Copying")
                    status = self.copy_file(f)
                    if status:
                        status, link = self.try_get_link(f)
            duration = time.time() - start
            if not status:
                warnings.append(f"{f}")
                self.logger.warning(f"Error occurred for file {f} {link}")
            else:
                self.logger.debug(f"got link {link} for file {f} in {duration} seconds")
                cache.append(f"{f};{link}")
        except Exception as e:
            self.logger.warning(f"Error occured for file {f} {e}")

    def update_cache(self):
        if not self.updating:
            self.update_thread = Thread(target=self.update_cache_helper)
            self.update_thread.start()
        else:
            self.logger.error("We are still updating")

    def update_cache_helper(self, fix_files=[]):
        if not self.updating_ev.is_set():
            self.updating_ev.set()
        if self.success_ev.is_set():
            self.success_ev.clear()
        if self.success_with_errors_ev.is_set():
            self.success_with_errors_ev.clear()
        self.logger.info(f"Updating local cache {self.cache_file}")
        init_cache_size = None
        try:
            warnings = []
            local_files, cache, remote_files = self.read_cache()
            if fix_files:
                for f in fix_files:
                    if f in cache:
                        cache.remove(f)
                remote_files = fix_files
            init_cache_size = len(cache)
            files = set(local_files) - set(remote_files)
            for f in files:
                if not self.updating_ev.is_set():
                    break
                self.get_link(f, cache, warnings)
            self.logger.info(f"Writing {len(cache) - init_cache_size} links to {self.cache_file}")
            shutil.copyfile(self.cache_file, self.cache_file + ".bak")
            with open(self.cache_file, "w") as cf:
                cf.write("\n".join(cache))
            self.updating_ev.clear()
            if warnings:
                self.success_with_errors_ev.set()
            else:
                self.success_ev.set()
        except Exception as e:
            self.updating_ev.clear()
            self.logger.error(f"Error {e} while updating cache")
            self.logger.error(f"Overwritten {self.cache_file}.\n" +
                              f"Original file backed up to {self.cache_file}.bak")
