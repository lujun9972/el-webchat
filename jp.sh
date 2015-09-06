#!/bin/bash - 
#===============================================================================
#
#          FILE: jp.sh
# 
#         USAGE: ./jp.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: YOUR NAME (), 
#  ORGANIZATION: 
#       CREATED: 2015年09月06日 22时01分47秒 CST
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
if [ $# -eq 0 ]
then
    filename=myscreen
else
    filename=$1
fi
xwd -frame |xwdtopnm|pnmtojpeg >$filename.jpeg

