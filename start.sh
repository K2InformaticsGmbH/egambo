#!/bin/bash

# Parameters:  NodeID ClusterID IMEMPort DDERLPort

nid=$1
cid=$2
port=$3
dderlport=$4
etcpjsonport=$5
if [ "$#" -ne 5 ]; then
    nid=1
    cid=2
    port=1236
    dderlport=8449
    etcpjsonport=8559
fi

unamestr=`uname`
host=127.0.0.1 # for local development.
name=egambo$nid@$host
cmname=egambo$cid@$host
imemtyp=disc
ck=egambo
if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
     exename=erl
else
    exename='start werl.exe'
    #exename='erl.exe'
fi

# Node name
node_name="-name $name"

# Cookie
cookie="-setcookie $ck"

# PATHS
paths="-pa"
paths=$paths" _checkouts/*/ebin"
paths=$paths" _build/default/lib/*/ebin"

# Proto dist module
dist_opts="-proto_dist"
dist_opts=$dist_opts" imem_inet_tcp"

# Kernel Opts
kernel_opts="-kernel"
kernel_opts=$kernel_opts" inet_dist_listen_min 7000"
kernel_opts=$kernel_opts" inet_dist_listen_max 7020"

# Imem Opts
imem_opts="-imem"
imem_opts=$imem_opts" mnesia_node_type $imemtyp"
imem_opts=$imem_opts" erl_cluster_mgrs ['$cmname']"
imem_opts=$imem_opts" mnesia_schema_name egambo"
imem_opts=$imem_opts" tcp_port $port"

# dderl opts
dderl_opts="-dderl"
dderl_opts=$dderl_opts" interface \"0.0.0.0\" port $dderlport"

# sasl opts
sasl_opts="-sasl"
sasl_opts=$sasl_opts"  sasl_error_logger false"

# etcpjson opts
etcpjson_opts="-etcpjson"
etcpjson_opts=$etcpjson_opts" interface \"0.0.0.0\" port $etcpjsonport"

# lager config
config="egambo.config"

start_opts="$paths $cookie $node_name $dist_opts $kernel_opts $imem_opts $dderl_opts $sasl_opts $etcpjson_opts -config $config"

# egambo start options
echo "------------------------------------------"
echo "Starting egambo (Opts)"
echo "------------------------------------------"
echo "Node Name : $node_name"
echo "Cookie    : $cookie"
echo "EBIN Path : $paths"
echo "Dist      : $dist_opts"
echo "Kernel    : $kernel_opts"
echo "IMEM      : $imem_opts"
echo "DDERL     : $dderl_opts"
echo "SASL      : $sasl_opts"
echo "ETCPJSON  : $etcpjson_opts"
echo "------------------------------------------"

# Starting egambo
echo $exename $start_opts -s egambo
$exename $start_opts -s egambo
