import os
import sys
import argparse


from . import __version__


def main():
    parser = argparse.ArgumentParser("ref-man")
    parser.add_argument("--no-threaded", dest="threaded", action="store_false",
                        help="Whether flask server should be threaded or not")
    parser.add_argument("--host", default="localhost",
                        help="host on which to bind the python server")
    parser.add_argument("--port", "-p", type=int, default=9999,
                        help="Port to bind to the python server")
    parser.add_argument("--proxy-port", dest="proxy_port", type=int, default=0,
                        help="HTTP proxy server port for method 'fetch_proxy'")
    parser.add_argument("--proxy-everything", dest="proxy_everything", action="store_true",
                        help="Should we proxy all requests?")
    parser.add_argument("--proxy-everything-port", dest="proxy_everything_port",
                        type=int, default=0,
                        help="HTTP proxy server port if proxy_everything is given")
    parser.add_argument("--data-dir", "-d", dest="data_dir", type=str,
                        default=os.path.expanduser("~"),
                        help="Semantic Scholar cache directory")
    parser.add_argument("--local-pdfs-dir", dest="local_pdfs_dir", type=str,
                        default=os.path.expanduser("~/pdfs"),
                        help="Local directory where pdfs are stored")
    parser.add_argument("--remote-pdfs-dir", dest="remote_pdfs_dir", type=str,
                        default="", help="Remote rclone pdfs directory")
    parser.add_argument("--remote-links-cache", dest="remote_links_cache", type=str,
                        default="", help="Remote links cache file")
    parser.add_argument("--batch-size", "-b", dest="batch_size", type=int, default=16,
                        help="Simultaneous connections to DBLP")
    parser.add_argument("--chrome-debugger-path", dest="chrome_debugger_path", type=str,
                        default="",
                        help="Path to chrome debugger script which can validate " +
                        "Semantic Scholar Search params (optional)")
    parser.add_argument("--verbosity", "-v", type=str, default="info",
                        help="Verbosity level. One of [error, info, debug]")
    parser.add_argument("--version", action="store_true",
                        help="Print version and exit.")
    args = parser.parse_args()
    if args.version:
        print(f"ref-man-server version {__version__}")
        sys.exit(0)
    from .server import Server
    kwargs = {"host": args.host,
              "port": args.port,
              "proxy_port": args.proxy_port,
              "proxy_everything": args.proxy_everything,
              "proxy_everything_port": args.proxy_everything_port,
              "data_dir": args.data_dir,
              "local_pdfs_dir": args.local_pdfs_dir,
              "remote_pdfs_dir": args.remote_pdfs_dir,
              "remote_links_cache": args.remote_links_cache,
              "batch_size": args.batch_size,
              "chrome_debugger_path": args.chrome_debugger_path,
              "verbosity": args.verbosity,
              "threaded": args.threaded}
    server = Server(**kwargs)
    server.run()


if __name__ == '__main__':
    main()
