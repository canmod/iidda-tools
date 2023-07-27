UVICON_PID = $(shell pgrep -f uvicorn)
runapp:
	uvicorn main:app &

stopapp:
	(kill -9 $(UVICON_PID) 2>/dev/null) || true

