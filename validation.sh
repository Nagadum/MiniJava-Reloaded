for f in tests/unitest/*.mjava
do
    echo "--------------------"
    echo $f
    foo=$(head -1 $f)
    echo $foo
    ./minijavac $f | grep -i 'Error\|File\|Result'
done
