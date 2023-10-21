package com.hunteryavitz.blockchainapi.services;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {"spring.profiles.active=test"})
@TestPropertySource(locations = "classpath:application-test.properties")
public class HealthMetricServiceTests {

    @Test
    void testConstructor() {
        HealthMetricService healthMetricService = new HealthMetricService();
        assert (healthMetricService.getBlockCount() == 0);
        assert (healthMetricService.getTransactionCount() == 0);
    }

    @Test
    void testIncrementBlockCount() {
        HealthMetricService healthMetricService = new HealthMetricService();
        healthMetricService.incrementBlockCount();
        assert (healthMetricService.getBlockCount() == 1);
    }

    @Test
    void testResetBlockCount() {
        HealthMetricService healthMetricService = new HealthMetricService();
        healthMetricService.incrementBlockCount();
        assert (healthMetricService.getBlockCount() == 1);
        healthMetricService.resetBlockCount();
        assert (healthMetricService.getBlockCount() == 0);
    }

    @Test
    void testIncrementTransactionCount() {
        HealthMetricService healthMetricService = new HealthMetricService();
        healthMetricService.incrementTransactionCount();
        assert (healthMetricService.getTransactionCount() == 1);
    }

    @Test
    void testResetTransactionCount() {
        HealthMetricService healthMetricService = new HealthMetricService();
        healthMetricService.incrementTransactionCount();
        assert (healthMetricService.getTransactionCount() == 1);
        healthMetricService.resetTransactionCount();
        assert (healthMetricService.getTransactionCount() == 0);
    }

    @Test
    void testUpdateBlockchainProduction() {
        HealthMetricService healthMetricService = new HealthMetricService();
        healthMetricService.updateBlockchainProduction();
        assert (healthMetricService.getBlockCount() == 0);
        assert (healthMetricService.getTransactionCount() == 0);
    }

    @Test
    void testGetProduction() {
        HealthMetricService healthMetricService = new HealthMetricService();
        assert (healthMetricService.getProduction() != null);
    }
}
