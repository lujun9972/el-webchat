#!/bin/bash - 

set -o nounset                              # Treat unset variables as an error
if [ $# -eq 0 ]
then
    filename=myscreen.png
else
    filename=$1.png
fi

function prog-exist-p 
{
    which $1;
}

if  prog-exist-p gnome-screenshot  
then
    echo "gnome-screenshot exist"
    gnome-screenshot -a -f $filename
elif prog-exist-p xwd
then
    echo "xwd exist"
    xwd -frame |xwdtopnm|pnmtopng >$filename
fi

