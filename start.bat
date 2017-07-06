@ECHO OFF

REM Parameters:  NodeID ClusterID IMEMPort DDERLPort

SET nid=%1
SET cid=%2
SET port=%3
SET dderlport=%4
SET etcpjsonport=%5

IF "%5" == "" (
   SET nid=1
   SET cid=2
   SET port=1236
   SET dderlport=8449
   SET etcpjsonport=8559
)

IF NOT "%5" == "" (
   SET nid=1
   SET cid=2
   SET port=1236
   SET dderlport=8449
   SET etcpjsonport=8559
)

SET unamestr=%USERNAME%
SET host=127.0.0.1
SET name=egambo%nid%@%host%
SET cmname=egambo%cid%@%host%
SET imemtyp=disc
SET ck=egambo

REM Node name
SET node_name=-name %name%

REM Cookie
SET cookie=-setcookie %ck%

REM PATHS
SET paths=-pa
SET paths=%paths% %cd%\_build\default\lib\cowboy\ebin
SET paths=%paths% %cd%\_build\default\lib\cowlib\ebin
SET paths=%paths% %cd%\_build\default\lib\dderl\ebin
SET paths=%paths% %cd%\_build\default\lib\egambo\ebin
SET paths=%paths% %cd%\_build\default\lib\erlimem\ebin
SET paths=%paths% %cd%\_build\default\lib\erlpkg\ebin
SET paths=%paths% %cd%\_build\default\lib\erlscrypt\ebin
SET paths=%paths% %cd%\_build\default\lib\esaml\ebin
SET paths=%paths% %cd%\_build\default\lib\etcpjson\ebin
SET paths=%paths% %cd%\_build\default\lib\goldrush\ebin
SET paths=%paths% %cd%\_build\default\lib\imem\ebin
SET paths=%paths% %cd%\_build\default\lib\jpparse\ebin
SET paths=%paths% %cd%\_build\default\lib\jsx\ebin
SET paths=%paths% %cd%\_build\default\lib\lager\ebin
SET paths=%paths% %cd%\_build\default\lib\mimetypes\ebin
SET paths=%paths% %cd%\_build\default\lib\ranch\ebin
SET paths=%paths% %cd%\_build\default\lib\sext\ebin
SET paths=%paths% %cd%\_build\default\lib\sqlparse\ebin

REM Proto dist module
SET dist_opts=-proto_dist
SET dist_opts=%dist_opts% imem_inet_tcp

REM Kernel Opts
SET kernel_opts=-kernel
SET kernel_opts=%kernel_opts% inet_dist_listen_min 7000
SET kernel_opts=%kernel_opts% inet_dist_listen_max 7020

SET Imem Opts
SET imem_opts=-imem
SET imem_opts=%imem_opts% mnesia_node_type %imemtyp%
SET imem_opts=%imem_opts% erl_cluster_mgrs ['%cmname%']
SET imem_opts=%imem_opts% mnesia_schema_name egambo
SET imem_opts=%imem_opts% tcp_port %port%

REM dderl opts
SET dderl_opts=-dderl
SET dderl_opts=%dderl_opts% interface '0.0.0.0' port %dderlport%

REM sasl opts
SET sasl_opts=-sasl
SET sasl_opts=%sasl_opts% sasl_error_logger false

REM etcpjson opts
SET etcpjson_opts=-etcpjson
SET etcpjson_opts=%etcpjson_opts% interface '0.0.0.0' port %etcpjsonport%

SET start_opts=%paths% %cookie% %node_name% %dist_opts% %kernel_opts% %imem_opts% %dderl_opts% %sasl_opts% %etcpjson_opts%

REM egambo start options
ECHO ------------------------------------------
ECHO Starting egambo (Opts)
ECHO ------------------------------------------
ECHO Node Name : %node_name%
ECHO Cookie    : %cookie%
ECHO EBIN Path : %paths%
ECHO Dist      : %dist_opts%
ECHO Kernel    : %kernel_opts%
ECHO IMEM      : %imem_opts%
ECHO DDERL     : %dderl_opts%
ECHO SASL      : %sasl_opts%
ECHO ETCPJSON  : %etcpjson_opts%
ECHO ------------------------------------------

REM Starting egambo
START /MAX werl %start_opts% -s egambo
