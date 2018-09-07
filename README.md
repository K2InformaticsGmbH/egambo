# Electronic Game Board
Bot programming platform for turn based games.

To connect from the outside you can use JSON over TCP with SSL. https://github.com/K2InformaticsGmbH/tcpjson
Follogwing there is a list with the supported commands:

```
 {"action": "login", "user": "player3", "password": "1"}
 {"action": "login", "user": "player3", "password": "abcd12345678"}

 {"action": "login", "user": "roman", "password": "qwerty123456"}


 {"action": "change_credentials", "user": "player3", "password": "", "new_password":1}
 {"action": "change_credentials", "user": "player3", "password": "", "new_password":"1"}
 {"action": "change_credentials", "user": "player3", "password": "", "new_password":"qwerty123456"}
 {"action": "change_credentials", "user": "player3", "password": "qwerty123456", "new_password":"abcd12345678"}


 {"action": "new_game", "type": "connect_four", "opponent": 1}
 {"action": "new_game", "type": "connect_four", "opponent": "bota"}
 {"action": "new_game", "type": "connect_four", "opponent": "bot1"}

 {"action": "new_game", "type": "connect_four"}
 {"action": "cancel", "game_id": 440240984908769341}

 {"action": "play", "game_id": 1283834649321306522, "position": 2}
```
Sample response format:
```
 {'msg': {'msg': {'aliases': [79, 88], 'board': '                                      X   ', 'etime': 1503393496, 'gravity': True, 'height': 6, 'id': 1270407400766756753, 'movers': [1, 3], 'periodic': False, 'run': 4, 'scores': [0, 0], 'status': 'playing', 'width': 7}, 'type': 'status'}} 
```
