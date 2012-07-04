<?php

class LispList {
    public $car;
    public $cdr;

    public function __construct($car, $cdr=null) {
        $this->car = $car;
        $this->cdr = $cdr;
    }

    private function stringify($thing) {
        switch(gettype($thing)) {
        case "string":
            return "\"$thing\"";
        case "null":
            return "nil";
        default:
            return (string)$thing;
        }
    }

    public function __toString() {
        $curr_list = $this;
        $result = "(";
        while(!is_null($curr_list)) {
            $result .= $this->stringify($this->car);
            if(listp($curr_list->cdr)) {
                $curr_list = $curr_list->cdr;
                if($curr_list) {
                    $result .= " ";
                }
            } else {
                $result .= " . " . $this->stringify($curr_list->cdr);
            }
        }
        $result .= ")";
        return $result;
    }
}


function listp($var) {
    return is_null($var) || ($var instanceof LispList);
}

function cons($car, $cdr) {
    return new LispList($car, $cdr);
}

function car($list) {
    if(is_null($list)) {
        return null;
    } elseif($list instanceof LispList) {
        return $list->car;
    } else {
        throw new Exception("CAR called with non-list as argument");
    }
}

function first($list) { return car($list); }

function cdr($list) {
    if(is_null($list)) {
        return null;
    } elseif($list instanceof LispList) {
        return $list->cdr;
    } else {
        throw new Exception("CDR called with non-list as argument");
    }
}

function rest($list) { return cdr($list); }

function list_() {
    $elems = func_get_args();
    $result = null;
    foreach(array_reverse($elems) as $elem) {
        $result = cons($elem, $result);
    }
    return $result;
}

function not($val) {
    if($val) {
        return null;
    } else {
        return true;
    }
}

// Numeric Comparison functions

function verify_number_args(&$args) {
    for($args as &$arg) {
        verify_number($arg);
    }
}

function verify_number(&$n) {
    if(is_string($n) || !is_numeric($n)) {
        throw new Exception(
            "Mathematical comparison made on non-number.");
    }
}

function equals() {
    $args = func_get_args();
    verify_number_args($args);
    for($i=1;$i<count($args);++$i) {
        if($args[$i - 1] != $args[$i]) {
            return null;
        }
    }
    return true;
}

function notequals() {
    $args = func_get_args();
    verify_number_args($args);
    for($i=1;$i<count($args);++$i) {
        if($args[$i - 1] == $args[$i]) {
            return null;
        }
    }
    return true;
}

function lessthan() {
    $args = func_get_args();
    verify_number_args($args);
    for($i=1;$i<count($args);++$i) {
        if($args[$i - 1] >= $args[$i]) {
            return null;
        }
    }
    return true;
}

function greaterthan() {
    $args = func_get_args();
    verify_number_args($args);
    for($i=1;$i<count($args);++$i) {
        if($args[$i - 1] <= $args[$i]) {
            return null;
        }
    }
    return true;
}

function lessthanequals() {
    $args = func_get_args();
    verify_number_args($args);
    for($i=1;$i<count($args);++$i) {
        if($args[$i - 1] > $args[$i]) {
            return null;
        }
    }
    return true;
}

function greaterthanequals() {
    $args = func_get_args();
    verify_number_args($args);
    for($i=1;$i<count($args);++$i) {
        if($args[$i - 1] < $args[$i]) {
            return null;
        }
    }
    return true;
}

