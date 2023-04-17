#!/bin/bash

UVICORN_PID=$(pgrep -f uvicorn)
if [[ -z ${UVICORN_PID} ]]
then
    echo "uvicorn is not running. starting it now ..."
else
    echo "killing existing uvicorn and starting a new one ..."
    kill -9 ${UVICORN_PID}
fi
uvicorn main:app&
