import multiprocessing


backlog = 2048
daemon = False
debug = False
spew = False
workers = multiprocessing.cpu_count() * 2 + 1
max_requests = 500
bind = "unix:/home/app/run/gunicorn.sock"
pidfile = "/home/app/run/gunicorn.pid"
logfile = "/home/app/run/gunicorn.log"
loglevel = "error"
user = "app"
proc_name = "ccfatigue"
timeout = 60
