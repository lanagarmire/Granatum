start:
	tmux new-session -s Granatum1 'cd ./server && node app.js'

stop:
	tmux kill-session -t Granatum1