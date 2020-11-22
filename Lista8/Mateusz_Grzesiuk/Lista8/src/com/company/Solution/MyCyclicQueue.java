package com.company.Solution;

import java.util.ArrayList;

public class MyCyclicQueue<E> implements MyQueue<E>{

    ArrayList<E> array;
    int start, end;

    public MyCyclicQueue(int n){
        array = new ArrayList<E>(n);
        while(n>0){
            array.add(null);
            n--;
        }
        start = 0;
        end = 0;
    }

    @Override
    public void enqueue(E x) throws FullException {
        if(array.get(end)==null){
            array.set(end, x);
            end=(end+1)%array.size();
        }
        else
            throw new FullException("Trying to add element to full Queue");
    }

    @Override
    public void dequeue() {
        if(array.get(start)!=null) {
            array.set(start, null);
            start=(start+1)%array.size();
        }
    }

    @Override
    public E first() throws EmptyException {
        if(array.get(start)!=null)
            return array.get(start);
        else
            throw new EmptyException("Trying to get first element of empty Queue");
    }

    @Override
    public boolean isEmpty() {
        return array.get(start)==null;
    }

    @Override
    public boolean isFull() {
        return array.get(end)!=null;
    }
}
