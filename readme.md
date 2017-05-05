gcc forth.c -o forth
./forth -c eforth.forth
cat bottles.forth | ./forth -i eforth.img
