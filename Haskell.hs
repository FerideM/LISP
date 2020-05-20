--������: ������: 3,10,12,13,26, ����: 1, �������:3

-- �3
--���������� �������, ������� �������� �������� ������ 
--�� ����� ����� �� ��� ������: 
--������ ������������� ����� � ������ ������������� �����.

sep :: [Integer] -> [[Integer]]
sep [] = []
sep(l) = [[x | x <- l, x > 0], [x | x <- l, x < 0]]


-- �10
--���������� �������, ������������� ������ (a b �) 
--� ���� (� (b (�))) � ��������.

transform (x:l) = (x, (head l,(tail l)))

transform1 (l) = (fst l, fst (snd l), snd (snd l ))


-- �12
--���������� �������, ����������� ������ (a b � d...) 
--�� ���� ((� b) (� d)...).

pairs [] = []
pairs (x:l) = (x, head l):(pairs (tail l))


-- �13
--���������� �������, �������, ������� �������� ������� 
--(a b...) � (1 2...), �������� ����� ������ (a 1 b 2 ...).

-- pairing :: [Int] -> [Int] -> [Int]
pairing [] [] = []
pairing (x:l)(y:m) = (x, y):(pairing l m)


-- �26
--����������� �������� ���������� ��������.

func [] = []
func [a] = [a]
func l = mergeSort (func (head (half l))) (func (head(tail (half l))))

half [] = []
half l = [take (n `div` 2) l] ++ [drop (n `div` 2) l]
       where n = length l

mergeSort x [] = x
mergeSort [] x = x
mergeSort (x:l) (y:m) = if x <= y
                        then x : mergeSort l (y:m)
                        else y : mergeSort (x:l) m
                        
                        
main = do
    print "___TASK 3___"
    print "Test 1"
    print $ sep [7, 18, -3, 0, 1]
    print "Test 2"
    print $ sep [-1, 4, 8, -6, -7, -3]
    print ""
    print "/////////////////////////////////"
    print ""
    print "___TASK 10___"
    print "Test 1"
    print $ transform [1, 2, 3]
    print "Test 2"
    print $ transform1 (1,(2,(3)))
    print ""
    print "/////////////////////////////////"
    print ""
    print "___TASK 12___"
    print "Test 1"
    print $ pairs [1, 2, 3, 4, 5, 6, 7, 8]
    print "Test 2"
    print $ pairs ["a","b","c","d","e","f"]
    print ""
    print "/////////////////////////////////"
    print ""
    print "___TASK 13___"
    print "Test 1"
    print $ pairing ["a","b","c","d","e","f","g"] [1, 2, 3, 4, 5, 6, 7]
    print "Test 2"
    print $ pairing [1, 2, 3, 4, 5, 6, 7] [7, 6, 5, 4, 3, 2, 1]
    print ""
    print "/////////////////////////////////"
    print ""
    print "___TASK 26___"
    print "Test 1"
    print $ func [7,11,3,8,4]
    print "Test 2"
    print $ func [4,6,-3,6,3,7,3,8,4,0,8,4,18,4]
    print "/////////////////////////////////"