package com.company.Test;

import com.company.Solution.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

public class MyCyclicQueueTest {

    @Test
    void SolutionTest() throws EmptyException, FullException {
        MyCyclicQueue<Integer> queue = new MyCyclicQueue<Integer>(3);
        assertTrue(queue.isEmpty());
        assertFalse(queue.isFull());
        assertThrows(EmptyException.class, queue::first);
        queue.enqueue(1);
        assertFalse(queue.isEmpty());
        assertFalse(queue.isFull());
        queue.enqueue(2);
        queue.enqueue(3);
        assertThrows(FullException.class, () ->  queue.enqueue(4));
        assertEquals(1, queue.first());
        queue.dequeue();
        queue.enqueue(5);
        assertEquals(2, queue.first());
        assertFalse(queue.isEmpty());
        assertTrue(queue.isFull());
        queue.dequeue();
        queue.dequeue();
        queue.dequeue();
        assertTrue(queue.isEmpty());
        assertFalse(queue.isFull());
        assertThrows(EmptyException.class, queue::first);
        queue.dequeue();
    }
}
