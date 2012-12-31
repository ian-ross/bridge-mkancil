#!/bin/sh
# the next line restarts using tclkit \
exec tclkit "$0" ${1+"$@"}
#exec /home/jeff/software/kbs/buildSunOS/bin/kbsmk8.5-dyn "$0" ${1+"$@"}
#exec /home/jeff/software/kbs/buildSunOS/bin/kbsvq8.5-dyn "$0" ${1+"$@"}

lappend auto_path /home/jeff/um/xancil
package require xancil
