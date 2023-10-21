package com.hunteryavitz.blockchainapi.utils.structures;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource(locations = "classpath:application-test.properties")
public class SlidingWindowTests {

    @Test
    public void testSlidingWindowShiftsCorrectly() {
        SlidingWindow slidingWindow = new SlidingWindow(3); // max size is 3
        slidingWindow.enqueueAndShift(1, 2);
        slidingWindow.enqueueAndShift(3, 4);
        slidingWindow.enqueueAndShift(5, 6);
        System.out.println("Size of the queue: " + slidingWindow.size());
        // log each value of slidingWindow
        System.out.println(slidingWindow.get(0, 0) + " " + slidingWindow.get(0, 1));
        System.out.println(slidingWindow.get(1, 0) + " " + slidingWindow.get(1, 1));
        System.out.println(slidingWindow.get(2, 0) + " " + slidingWindow.get(2, 1));

        slidingWindow.enqueueAndShift(7, 8); // This will cause (1, 2) to be removed
        System.out.println("Size of the queue: " + slidingWindow.size());
        System.out.println(slidingWindow.get(0, 0) + " " + slidingWindow.get(0, 1));
        System.out.println(slidingWindow.get(1, 0) + " " + slidingWindow.get(1, 1));
        System.out.println(slidingWindow.get(2, 0) + " " + slidingWindow.get(2, 1));
        assert !slidingWindow.isEmpty();
    }

}
