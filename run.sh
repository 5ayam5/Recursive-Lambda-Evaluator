make
for file in testcases/*
do
	echo $file
	./a2 $file
	echo
done
