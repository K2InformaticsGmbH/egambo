#!/bin/bash

# Change below to use specific hosts
node_host=127.0.0.1
cluster_host=127.0.0.1

app_name=egambo
node_name=egambo
cluster_name=egambo
cookie=egambo

dderlip=$node_host
dderlport=8449
imemip=$node_host
imemport=1236
imemtype=ram
imemschema=$app_name
config=$PWD/sbsgui.config

cmd=$0
argscount=$#
function usage {
    echo "usage: $1 add"
    echo "       $1 remove"
    echo "       $1 start"
    echo "       $1 stop"
    echo "       $1 list"
    echo "       $1 attach"
    echo "       $1 gui"
    echo "       $1 txt"
    echo "       $1 check"
    echo "       $1 escript"
    echo "default arguments app_name = $app_name"
    echo "                  node_name = $node_name"
    echo "                  node_host = $node_host"
    echo "                  cluster_name = $node_name"
    echo "                  cluster_host = $cluster_host"
    echo "                  cookie = $cookie"
}

if [ "$1" == "gui" ] ||
   [ "$1" == "txt" ] ||
   [ "$1" == "add" ] ||
   [ "$1" == "remove" ] ||
   [ "$1" == "start" ] ||
   [ "$1" == "stop" ] ||
   [ "$1" == "list" ]; then
    ./_build/default/lib/dderl/service_generic.sh $1 $app_name $node_name $node_host $cluster_name $cluster_host $cookie $dderlip $dderlport $imemip $imemport $imemtype $imemschema $config
elif [ "$1" == "attach" ]; then
    ./_build/default/lib/dderl/service_generic.sh $1 $node_name $node_host $cookie
elif [ "$1" == "escript" ]; then
    ./_build/default/lib/dderl/service_generic.sh $1 $2 $node_name $node_host $cookie
elif [ "$1" == "check" ]; then
    ./_build/default/lib/dderl/service_generic.sh $1 $node_name $node_host $cookie
else
    usage $cmd
    echo ---------------
    echo Generic usage :
    echo
    ./deps/dderl/service_generic.sh
fi
