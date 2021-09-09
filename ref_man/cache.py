import logging
from pathlib import Path
import os
import time
import shutil
from subprocess import Popen, PIPE, TimeoutExpired
from threading import Thread, Event


class CacheHelper:
    def __init__(self, local_dir: Path, remote_dir: Path,
                 cache_file: Path, logger: logging.Logger):
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
    def updating(self) -> bool:
        return self.updating_ev.is_set()

    @property
    def finished(self) -> bool:
        return self.success_ev.is_set()

    @property
    def finished_with_errors(self) -> bool:
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

    def _remote_path(self, fname):
        return os.path.join(self.remote_dir, os.path.basename(fname))

    def _local_path(self, fname):
        return os.path.join(self.local_dir, os.path.basename(fname))

    def stop_update(self):
        self.updating_ev.clear()

    def shutdown(self):
        self.stop_update()
        if self.update_thread is not None:
            self.update_thread.join()

    def check_and_fix_cache(self):
        self.logger.debug("Checking existing cache")
        local_files, cache, remote_files = self.read_cache()
        deleted_links = [c for c in cache if not(os.path.exists(c.split(";")[0]))]
        deleted_files = [c.split(";")[0] for c in deleted_links]
        if deleted_links:
            self.logger.info(f"Files {deleted_files} not on disk. Removing from cache.")
            for f in deleted_links:
                cache.remove(f)
            with open(self.cache_file, "w") as f:
                f.write("\n".join(cache))
        broken_links = [c.split(";")[0] for c in cache if c.split(";")[1] == ""]
        if broken_links:
            self.logger.debug(f"Found {len(broken_links)} broken links. Updating")
            self.update_thread = Thread(target=self.update_cache_helper, args=[broken_links])
            self.update_thread.start()
        else:
            self.logger.debug(f"No broken links found")

    def copy_file(self, fname):
        local_path = self._local_path(fname)
        try:
            p = Popen(f"rclone --no-update-modtime -v copy {local_path} {self.remote_dir}",
                      shell=True, stdout=PIPE, stderr=PIPE)
            out, err = p.communicate(timeout=10)
            err = err.decode("utf-8").lower()
            if err and ("copied" in err or "transferred" in err):
                self.logger.debug(f"Copied file {local_path} to remote")
                status = True
            else:
                status = False
        except TimeoutExpired:
            self.logger.warning(f"Timeout while copying for file {local_path}")
            status = False
        return status

    def try_get_link(self, remote_path):
        self.logger.debug(f"Fetching link for {remote_path}")
        try:
            p = Popen(f"rclone -v link {remote_path}", shell=True, stdout=PIPE, stderr=PIPE)
            out, err = p.communicate(timeout=10)
            if err:
                if "error 403" in err.decode().lower() or\
                   "object not found" in err.decode().lower():
                    status = False
                    link = "NOT_PRESENT"
                else:
                    status = False
                    link = "OTHER_ERROR. {err.decode('utf-8')}"
            else:
                link = out.decode("utf-8").replace("\n", "")
                if link:
                    status = True
                else:
                    status = False
                    link = "EMPTY_RESPONSE"
        except TimeoutExpired:
            self.logger.warning(f"Timeout while getting link for file {remote_path}")
            link = "TIMEOUT"
            status = False
        return status, link

    def get_link(self, fname, cache, warnings):
        try:
            start = time.time()
            remote_path = self._remote_path(fname)
            if " " in remote_path:
                remote_path = f'"{remote_path}"'
            status, link = self.try_get_link(remote_path)
            if not status:
                if link == "NOT_PRESENT":
                    self.logger.warning(f"File {fname} does not exist on remote. Copying")
                    status = self.copy_file(fname)
                    if status:
                        status, link = self.try_get_link(remote_path)
                else:
                    raise ValueError(f"Error {link} for {remote_path}")
            duration = time.time() - start
            if not status:
                warnings.append(f"{fname}")
                self.logger.error(f"Error occurred for file {fname} {link}")
            else:
                self.logger.debug(f"got link {link} for file {fname} in {duration} seconds")
                cache[fname] = link
        except Exception as e:
            self.logger.error(f"Error occured for file {fname} {e}")

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
        try:
            warnings = []
            local_files, cache, remote_files = self.read_cache()
            files = set(local_files) - set(remote_files)
            if fix_files:
                for f in fix_files:
                    if f in cache:
                        cache.remove(f)
                files = fix_files
            init_cache_size = len(cache)
            cache = dict(c.split(";") for c in cache)
            self.logger.info(f"Will try to fetch links for {len(files)} files")
            for f in files:
                if not self.updating_ev.is_set():
                    break
                self.get_link(f, cache, warnings)
            self.logger.info(f"Writing {len(cache) - init_cache_size} links to {self.cache_file}")
            shutil.copyfile(self.cache_file, self.cache_file + ".bak")
            with open(self.cache_file, "w") as cf:
                write_list = [";".join(c) for c in cache.items()]
                cf.write("\n".join(write_list))
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
