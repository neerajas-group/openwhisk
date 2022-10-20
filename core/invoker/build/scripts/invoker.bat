@rem
@rem Copyright 2015 the original author or authors.
@rem
@rem Licensed under the Apache License, Version 2.0 (the "License");
@rem you may not use this file except in compliance with the License.
@rem You may obtain a copy of the License at
@rem
@rem      https://www.apache.org/licenses/LICENSE-2.0
@rem
@rem Unless required by applicable law or agreed to in writing, software
@rem distributed under the License is distributed on an "AS IS" BASIS,
@rem WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
@rem See the License for the specific language governing permissions and
@rem limitations under the License.
@rem

@if "%DEBUG%" == "" @echo off
@rem ##########################################################################
@rem
@rem  invoker startup script for Windows
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

set DIRNAME=%~dp0
if "%DIRNAME%" == "" set DIRNAME=.
set APP_BASE_NAME=%~n0
set APP_HOME=%DIRNAME%..

@rem Resolve any "." and ".." in APP_HOME to make it shorter.
for %%i in ("%APP_HOME%") do set APP_HOME=%%~fi

@rem Add default JVM options here. You can also use JAVA_OPTS and INVOKER_OPTS to pass JVM options to this script.
set DEFAULT_JVM_OPTS=

@rem Find java.exe
if defined JAVA_HOME goto findJavaFromJavaHome

set JAVA_EXE=java.exe
%JAVA_EXE% -version >NUL 2>&1
if "%ERRORLEVEL%" == "0" goto execute

echo.
echo ERROR: JAVA_HOME is not set and no 'java' command could be found in your PATH.
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:findJavaFromJavaHome
set JAVA_HOME=%JAVA_HOME:"=%
set JAVA_EXE=%JAVA_HOME%/bin/java.exe

if exist "%JAVA_EXE%" goto execute

echo.
echo ERROR: JAVA_HOME is set to an invalid directory: %JAVA_HOME%
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:execute
@rem Setup the command line

set CLASSPATH=%APP_HOME%\lib\openwhisk-invoker-1.0.0-SNAPSHOT.jar;%APP_HOME%\lib\openwhisk-scheduler-1.0.0-SNAPSHOT.jar;%APP_HOME%\lib\openwhisk-common-1.0.0-SNAPSHOT.jar;%APP_HOME%\lib\akka-stream-alpakka-file_2.12-1.1.2.jar;%APP_HOME%\lib\akka-stream-alpakka-s3_2.12-1.1.2.jar;%APP_HOME%\lib\akka-grpc-runtime_2.12-1.0.0.jar;%APP_HOME%\lib\akka-discovery-kubernetes-api_2.12-1.0.5.jar;%APP_HOME%\lib\akka-cluster-metrics_2.12-2.6.12.jar;%APP_HOME%\lib\akka-cluster-tools_2.12-2.6.12.jar;%APP_HOME%\lib\akka-distributed-data_2.12-2.6.12.jar;%APP_HOME%\lib\akka-management-cluster-bootstrap_2.12-1.0.5.jar;%APP_HOME%\lib\akka-cluster_2.12-2.6.12.jar;%APP_HOME%\lib\akka-kryo-serialization_2.12-1.0.0.jar;%APP_HOME%\lib\akka-remote_2.12-2.6.12.jar;%APP_HOME%\lib\akka-management_2.12-1.0.5.jar;%APP_HOME%\lib\akka-stream_2.12-2.6.12.jar;%APP_HOME%\lib\akka-slf4j_2.12-2.6.12.jar;%APP_HOME%\lib\akka-coordination_2.12-2.6.12.jar;%APP_HOME%\lib\akka-discovery_2.12-2.6.12.jar;%APP_HOME%\lib\akka-pki_2.12-2.6.12.jar;%APP_HOME%\lib\akka-actor_2.12-2.6.12.jar;%APP_HOME%\lib\akka-http-spray-json_2.12-10.2.4.jar;%APP_HOME%\lib\akka-http-xml_2.12-10.2.4.jar;%APP_HOME%\lib\akka-http_2.12-10.2.4.jar;%APP_HOME%\lib\akka-http2-support_2.12-10.2.4.jar;%APP_HOME%\lib\akka-http-core_2.12-10.2.4.jar;%APP_HOME%\lib\akka-parsing_2.12-10.2.4.jar;%APP_HOME%\lib\pureconfig_2.12-0.11.1.jar;%APP_HOME%\lib\kamon-statsd_2.12-2.1.12.jar;%APP_HOME%\lib\kamon-system-metrics_2.12-2.1.12.jar;%APP_HOME%\lib\kamon-prometheus_2.12-2.1.12.jar;%APP_HOME%\lib\kamon-datadog_2.12-2.1.12.jar;%APP_HOME%\lib\kamon-core_2.12-2.1.12.jar;%APP_HOME%\lib\elastic4s-http_2.12-6.7.4.jar;%APP_HOME%\lib\mongo-scala-driver_2.12-2.7.0.jar;%APP_HOME%\lib\pureconfig-generic_2.12-0.11.1.jar;%APP_HOME%\lib\pureconfig-core_2.12-0.11.1.jar;%APP_HOME%\lib\scala-java8-compat_2.12-0.8.0.jar;%APP_HOME%\lib\ssl-config-core_2.12-0.4.2.jar;%APP_HOME%\lib\spray-json_2.12-1.3.6.jar;%APP_HOME%\lib\elastic4s-core_2.12-6.7.4.jar;%APP_HOME%\lib\exts_2.12-1.61.1.jar;%APP_HOME%\lib\jackson-module-scala_2.12-2.10.0.jar;%APP_HOME%\lib\mongo-scala-bson_2.12-2.7.0.jar;%APP_HOME%\lib\scala-reflect-2.12.8.jar;%APP_HOME%\lib\scalapb-runtime_2.12-0.10.4.jar;%APP_HOME%\lib\pureconfig-macros_2.12-0.11.1.jar;%APP_HOME%\lib\shapeless_2.12-2.3.3.jar;%APP_HOME%\lib\scala-parser-combinators_2.12-1.1.2.jar;%APP_HOME%\lib\scala-xml_2.12-1.3.0.jar;%APP_HOME%\lib\lenses_2.12-0.10.4.jar;%APP_HOME%\lib\scala-collection-compat_2.12-2.1.6.jar;%APP_HOME%\lib\macro-compat_2.12-1.1.1.jar;%APP_HOME%\lib\scala-library-2.12.12.jar;%APP_HOME%\lib\curator-recipes-4.0.0.jar;%APP_HOME%\lib\zookeeper-3.4.11.jar;%APP_HOME%\lib\fastparse_2.12-2.3.0.jar;%APP_HOME%\lib\logback-classic-1.2.3.jar;%APP_HOME%\lib\jcl-over-slf4j-1.7.25.jar;%APP_HOME%\lib\log4j-over-slf4j-1.7.25.jar;%APP_HOME%\lib\elasticsearch-rest-client-6.7.2.jar;%APP_HOME%\lib\aws-java-sdk-cloudfront-1.11.517.jar;%APP_HOME%\lib\aws-java-sdk-core-1.11.517.jar;%APP_HOME%\lib\httpclient-4.5.5.jar;%APP_HOME%\lib\commons-codec-1.10.jar;%APP_HOME%\lib\azure-cosmosdb-2.6.2.jar;%APP_HOME%\lib\azure-cosmosdb-direct-2.6.2.jar;%APP_HOME%\lib\azure-cosmosdb-gateway-2.6.2.jar;%APP_HOME%\lib\azure-cosmosdb-commons-2.6.2.jar;%APP_HOME%\lib\commons-io-2.6.jar;%APP_HOME%\lib\commons-collections-3.2.2.jar;%APP_HOME%\lib\kafka-clients-2.4.0.jar;%APP_HOME%\lib\java-uuid-generator-3.1.4.jar;%APP_HOME%\lib\caffeine-2.6.2.jar;%APP_HOME%\lib\etcd-java-0.0.13.jar;%APP_HOME%\lib\grpc-stub-1.30.0.jar;%APP_HOME%\lib\grpc-netty-1.24.1.jar;%APP_HOME%\lib\grpc-netty-shaded-1.30.0.jar;%APP_HOME%\lib\grpc-core-1.30.0.jar;%APP_HOME%\lib\grpc-protobuf-1.24.1.jar;%APP_HOME%\lib\grpc-protobuf-lite-1.24.1.jar;%APP_HOME%\lib\grpc-api-1.30.0.jar;%APP_HOME%\lib\curator-framework-4.0.0.jar;%APP_HOME%\lib\curator-client-4.0.0.jar;%APP_HOME%\lib\guava-28.2-android.jar;%APP_HOME%\lib\perfmark-api-0.19.0.jar;%APP_HOME%\lib\jsr305-3.0.2.jar;%APP_HOME%\lib\kubernetes-client-4.10.3.jar;%APP_HOME%\lib\opentracing-util-0.31.0.jar;%APP_HOME%\lib\brave-opentracing-0.31.0.jar;%APP_HOME%\lib\opentracing-noop-0.31.0.jar;%APP_HOME%\lib\opentracing-api-0.31.0.jar;%APP_HOME%\lib\zipkin-sender-okhttp3-2.6.1.jar;%APP_HOME%\lib\brave-5.0.0.jar;%APP_HOME%\lib\zipkin-reporter-2.6.1.jar;%APP_HOME%\lib\rxjava-reactive-streams-1.2.1.jar;%APP_HOME%\lib\rxjava-extras-0.8.0.17.jar;%APP_HOME%\lib\rxjava-string-1.1.1.jar;%APP_HOME%\lib\rxnetty-0.4.20.jar;%APP_HOME%\lib\rxjava-1.3.8.jar;%APP_HOME%\lib\azure-storage-blob-12.7.0.jar;%APP_HOME%\lib\audience-annotations-0.5.0.jar;%APP_HOME%\lib\netty-3.10.5.Final.jar;%APP_HOME%\lib\sourcecode_2.12-0.2.1.jar;%APP_HOME%\lib\geny_2.12-0.6.0.jar;%APP_HOME%\lib\config-1.4.0.jar;%APP_HOME%\lib\akka-protobuf-v3_2.12-2.6.12.jar;%APP_HOME%\lib\azure-storage-common-12.7.0.jar;%APP_HOME%\lib\azure-core-http-netty-1.5.2.jar;%APP_HOME%\lib\azure-core-1.5.1.jar;%APP_HOME%\lib\reactor-netty-0.9.7.RELEASE.jar;%APP_HOME%\lib\reactor-core-3.3.5.RELEASE.jar;%APP_HOME%\lib\reactive-streams-1.0.3.jar;%APP_HOME%\lib\oshi-core-5.3.6.jar;%APP_HOME%\lib\metrics-core-4.1.0.jar;%APP_HOME%\lib\slf4j-api-1.7.30.jar;%APP_HOME%\lib\lmdbjava-0.7.0.jar;%APP_HOME%\lib\hpack-1.0.2.jar;%APP_HOME%\lib\logback-core-1.2.3.jar;%APP_HOME%\lib\zstd-jni-1.4.3-1.jar;%APP_HOME%\lib\lz4-java-1.6.0.jar;%APP_HOME%\lib\snappy-java-1.1.7.3.jar;%APP_HOME%\lib\httpcore-4.4.9.jar;%APP_HOME%\lib\kubernetes-model-rbac-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-admissionregistration-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-apps-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-autoscaling-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-apiextensions-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-batch-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-certificates-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-coordination-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-discovery-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-events-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-extensions-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-networking-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-metrics-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-policy-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-scheduling-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-settings-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-storageclass-4.10.3.jar;%APP_HOME%\lib\openshift-model-4.10.3.jar;%APP_HOME%\lib\kubernetes-model-core-4.10.3.jar;%APP_HOME%\lib\logging-interceptor-3.12.12.jar;%APP_HOME%\lib\okhttp-3.14.7.jar;%APP_HOME%\lib\jackson-dataformat-yaml-2.10.3.jar;%APP_HOME%\lib\jackson-datatype-jsr310-2.10.3.jar;%APP_HOME%\lib\zjsonpatch-0.3.0.jar;%APP_HOME%\lib\jmespath-java-1.11.517.jar;%APP_HOME%\lib\kubernetes-model-common-4.10.3.jar;%APP_HOME%\lib\jackson-dataformat-xml-2.10.1.jar;%APP_HOME%\lib\jackson-module-jaxb-annotations-2.10.3.jar;%APP_HOME%\lib\jackson-module-paranamer-2.10.0.jar;%APP_HOME%\lib\jackson-databind-2.10.3.jar;%APP_HOME%\lib\jackson-dataformat-cbor-2.6.7.jar;%APP_HOME%\lib\jackson-core-2.10.3.jar;%APP_HOME%\lib\generex-1.0.2.jar;%APP_HOME%\lib\nanojson-1.6.jar;%APP_HOME%\lib\netty-transport-native-epoll-4.1.49.Final-linux-x86_64.jar;%APP_HOME%\lib\netty-tcnative-boringssl-static-2.0.29.Final.jar;%APP_HOME%\lib\gson-2.8.6.jar;%APP_HOME%\lib\zipkin-2.8.4.jar;%APP_HOME%\lib\netty-handler-proxy-4.1.49.Final.jar;%APP_HOME%\lib\netty-codec-http2-4.1.49.Final.jar;%APP_HOME%\lib\netty-codec-http-4.1.49.Final.jar;%APP_HOME%\lib\netty-handler-4.1.49.Final.jar;%APP_HOME%\lib\netty-transport-native-unix-common-4.1.49.Final.jar;%APP_HOME%\lib\netty-codec-socks-4.1.49.Final.jar;%APP_HOME%\lib\netty-codec-4.1.49.Final.jar;%APP_HOME%\lib\netty-transport-4.1.49.Final.jar;%APP_HOME%\lib\commons-text-1.6.jar;%APP_HOME%\lib\commons-lang3-3.8.1.jar;%APP_HOME%\lib\mongodb-driver-async-3.11.0.jar;%APP_HOME%\lib\error_prone_annotations-2.3.4.jar;%APP_HOME%\lib\animal-sniffer-annotations-1.18.jar;%APP_HOME%\lib\kryo-4.0.2.jar;%APP_HOME%\lib\agrona-1.8.0.jar;%APP_HOME%\lib\jnr-ffi-2.1.9.jar;%APP_HOME%\lib\jffi-1.2.18.jar;%APP_HOME%\lib\jffi-1.2.18-native.jar;%APP_HOME%\lib\jnr-constants-0.9.12.jar;%APP_HOME%\lib\snakeyaml-1.24.jar;%APP_HOME%\lib\jackson-annotations-2.10.3.jar;%APP_HOME%\lib\automaton-1.11-8.jar;%APP_HOME%\lib\jna-platform-5.6.0.jar;%APP_HOME%\lib\jna-5.6.0.jar;%APP_HOME%\lib\okio-1.17.2.jar;%APP_HOME%\lib\netty-buffer-4.1.49.Final.jar;%APP_HOME%\lib\netty-resolver-4.1.49.Final.jar;%APP_HOME%\lib\netty-common-4.1.49.Final.jar;%APP_HOME%\lib\protobuf-java-3.11.4.jar;%APP_HOME%\lib\proto-google-common-protos-1.12.0.jar;%APP_HOME%\lib\commons-collections4-4.2.jar;%APP_HOME%\lib\micrometer-core-1.2.0.jar;%APP_HOME%\lib\joda-time-2.9.9.jar;%APP_HOME%\lib\httpasyncclient-4.1.2.jar;%APP_HOME%\lib\httpcore-nio-4.4.5.jar;%APP_HOME%\lib\mongodb-driver-core-3.11.0.jar;%APP_HOME%\lib\bson-3.11.0.jar;%APP_HOME%\lib\ion-java-1.0.2.jar;%APP_HOME%\lib\annotations-4.1.1.4.jar;%APP_HOME%\lib\failureaccess-1.0.1.jar;%APP_HOME%\lib\listenablefuture-9999.0-empty-to-avoid-conflict-with-guava.jar;%APP_HOME%\lib\checker-compat-qual-2.5.5.jar;%APP_HOME%\lib\j2objc-annotations-1.3.jar;%APP_HOME%\lib\grpc-context-1.30.0.jar;%APP_HOME%\lib\reflectasm-1.11.3.jar;%APP_HOME%\lib\minlog-1.3.0.jar;%APP_HOME%\lib\objenesis-2.5.1.jar;%APP_HOME%\lib\asn-one-0.5.0.jar;%APP_HOME%\lib\asm-commons-5.0.3.jar;%APP_HOME%\lib\asm-analysis-5.0.3.jar;%APP_HOME%\lib\asm-util-5.0.3.jar;%APP_HOME%\lib\asm-tree-5.0.3.jar;%APP_HOME%\lib\asm-5.0.4.jar;%APP_HOME%\lib\jnr-a64asm-1.0.0.jar;%APP_HOME%\lib\jnr-x86asm-1.0.2.jar;%APP_HOME%\lib\jakarta.xml.bind-api-2.3.2.jar;%APP_HOME%\lib\jakarta.activation-api-1.2.1.jar;%APP_HOME%\lib\HdrHistogram-2.1.11.jar;%APP_HOME%\lib\LatencyUtils-2.0.3.jar;%APP_HOME%\lib\paranamer-2.8.jar;%APP_HOME%\lib\woodstox-core-6.0.2.jar;%APP_HOME%\lib\stax2-api-4.2.jar


@rem Execute invoker
"%JAVA_EXE%" %DEFAULT_JVM_OPTS% %JAVA_OPTS% %INVOKER_OPTS%  -classpath "%CLASSPATH%" org.apache.openwhisk.core.invoker.Invoker %*

:end
@rem End local scope for the variables with windows NT shell
if "%ERRORLEVEL%"=="0" goto mainEnd

:fail
rem Set variable INVOKER_EXIT_CONSOLE if you need the _script_ return code instead of
rem the _cmd.exe /c_ return code!
if  not "" == "%INVOKER_EXIT_CONSOLE%" exit 1
exit /b 1

:mainEnd
if "%OS%"=="Windows_NT" endlocal

:omega
