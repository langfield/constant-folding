import re
import ast
import collections
from typing import *
from loguru import logger

class TreeNode:
    def __init__(self, x: int, l: 'TreeNode | None' = None, r: 'TreeNode | None' = None):
        self.val = x
        self.left = l
        self.right = r

solution = Solution()
while 1:
    x = ast.literal_eval(input())
    print(solution(x))
