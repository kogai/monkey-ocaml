BIN=./monkey.byte
DIR=$(find -f fixture/*.ko)

for d in $DIR; do
 $BIN "$d"
done
