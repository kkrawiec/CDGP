#!/bin/bash

echo "Arguments" $0

for file in `ls | grep .sl`
do
	fileOut="${file}2"
	echo 'cleaning: ' $file
	echo 'file to save: ' $fileOut

	tr '\r\n' '\n' < $file > $fileOut
	
	rm $file
	mv $fileOut $file
done
