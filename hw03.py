HW_SOURCE_FILE = __file__


def num_eights(pos):
    """Returns the number of times 8 appears as a digit of pos.

    >>> num_eights(3)
    0
    >>> num_eights(8)
    1
    >>> num_eights(88888888)
    8
    >>> num_eights(2638)
    1
    >>> num_eights(86380)
    2
    >>> num_eights(12345)
    0
    >>> from construct_check import check
    >>> # ban all assignment statements
    >>> check(HW_SOURCE_FILE, 'num_eights',
    ...       ['Assign', 'AnnAssign', 'AugAssign', 'NamedExpr'])
    True
    """
    "*** YOUR CODE HERE ***"
    if pos%10==8:
        return 1 + num_eights(pos//10)
    elif pos==False:
        return 0
    else:
        return num_eights(pos//10)
def pingpong(n):
    """Return the nth element of the ping-pong sequence.

    >>> pingpong(8)
    8
    >>> pingpong(10)
    6
    >>> pingpong(15)
    1
    >>> pingpong(21)
    -1
    >>> pingpong(22)
    -2
    >>> pingpong(30)
    -2
    >>> pingpong(68)
    0
    >>> pingpong(69)
    -1
    >>> pingpong(80)
    0
    >>> pingpong(81)
    1
    >>> pingpong(82)
    0
    >>> pingpong(100)
    -6
    >>> from construct_check import check
    >>> # ban assignment statements
    >>> check(HW_SOURCE_FILE, 'pingpong',
    ...       ['Assign', 'AnnAssign', 'AugAssign', 'NamedExpr'])
    True
    """
    
    "*** YOUR CODE HERE ***"
    
    def sign(countess, answer):
        if countess%2==0:
            return answer-1
        else:
            return answer+1
    def loop(answer, n, index, countess):
        if index==n:
            return answer
        elif num_eights(index) != 0:
            return loop(sign(countess+1, answer), n, index+1, countess+1)
        elif index%8==0:
            return loop(sign(countess+1, answer), n, index+1, countess+1)
        else:
            return loop(sign(countess, answer), n, index+1, countess)
    return loop(0, n, 0, 0)





        





    


def get_larger_coin(coin):
    """Returns the next larger coin in order.
    >>> get_larger_coin(1)
    5
    >>> get_larger_coin(5)
    10
    >>> get_larger_coin(10)
    25
    >>> get_larger_coin(2) # Other values return None
    """
    if coin == 1:
        return 5
    elif coin == 5:
        return 10
    elif coin == 10:
        return 25


def get_smaller_coin(coin):
    """Returns the next smaller coin in order.
    >>> get_smaller_coin(25)
    10
    >>> get_smaller_coin(10)
    5
    >>> get_smaller_coin(5)
    1
    >>> get_smaller_coin(2) # Other values return None
    """
    if coin == 25:
        return 10
    elif coin == 10:
        return 5
    elif coin == 5:
        return 1


def count_coins(change):
    """Return the number of ways to make change using coins of value of 1, 5, 10, 25.
    >>> count_coins(15)
    6
    >>> count_coins(10)
    4
    >>> count_coins(20)
    9
    >>> count_coins(100) # How many ways to make change for a dollar?
    242
    >>> count_coins(200)
    1463
    >>> from construct_check import check
    >>> # ban iteration
    >>> check(HW_SOURCE_FILE, 'count_coins', ['While', 'For'])
    True
    """
    "*** YOUR CODE HERE ***"
    #def variety(coin):
    coin=1
    def revolve(change, count, coin):
        if change==count:
            return 1
        elif count>change:
            return 0
        else:
            if coin==1:
                return revolve(change, count+coin, coin) + revolve(change, count+get_larger_coin(coin), get_larger_coin(coin))
            elif coin==25:
                return revolve(change, count+coin, coin) + revolve(change, count+get_smaller_coin(coin), get_smaller_coin(coin))
            else:
                return revolve(change, count+coin, coin) + revolve(change, count+get_larger_coin(coin), get_larger_coin(coin)) + revolve(change, count+get_smaller_coin(coin), get_smaller_coin(coin))
    if coin != 25:
        return revolve(change, 0, coin) + revolve(change, 0, get_larger_coin(coin))
    else:
        return revolve(change, 0, coin)

    
    
    #return variety(1)
