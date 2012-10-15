#!/bin/bash

cd ~/projects/cocos2d/HelloWorld/proj.linux/bin/debug/
./HelloCpp &

sleep 0.2

nc localhost 1234
