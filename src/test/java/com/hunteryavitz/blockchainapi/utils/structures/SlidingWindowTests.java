package com.hunteryavitz.blockchainapi.utils.structures;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource(locations = "classpath:application-test.properties")
public class SlidingWindowTests {

    @Test
    public void testSlidingWindowShiftsCorrectly() {
        SlidingWindow slidingWindow = new SlidingWindow(3);
        slidingWindow.enqueueAndShift(1, 2);
        slidingWindow.enqueueAndShift(3, 4);
        slidingWindow.enqueueAndShift(5, 6);
        slidingWindow.enqueueAndShift(7, 8);
        assert !slidingWindow.isEmpty();
    }
}
