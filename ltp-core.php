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
