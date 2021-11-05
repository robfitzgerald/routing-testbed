#!/bin/bash

protoc --java_out=../core/src/main/java external_assignment_v1.proto
protoc --python_out=../python/relay_server/proto external_assignment_v1.proto