#!/bin/sh

ligo dry-run src/test/contracts/crowdfunding.ligo main Back 'test_back' --predecessor-timestamp="2020-10-05T10:00:00Z"
# failwith("Deadline passed.")

ligo dry-run src/test/contracts/crowdfunding.ligo main Back 'test_back' --predecessor-timestamp="2020-03-05T10:00:00Z"
# ( list[] , record[ 
#     backers -> map[
#       @"tz1gkYy4tGWFsgqKS6bWjF7Didgd7Zips46t" -> 0mutez , 
#       @"tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc" -> 2000000mutez , 
#       @"tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA" -> 1000000mutez
#     ] ,
#     deadline -> +1583834400 ,
#     funded -> false ,
#     owner -> @"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" ,
#     target -> 100000000mutez
# ] )