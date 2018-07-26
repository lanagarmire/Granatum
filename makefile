start:
	tmux new-session -s Granatum1 'cd ./server && node app.js'

stop:
	tmux kill-session -t Granatum1

format:
	find R -maxdepth 1 -type f -exec ./tidy.R {} \;
	# todo: add all R files