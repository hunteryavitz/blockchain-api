package com.hunteryavitz.blockchainapi.controllers.blockchain;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

/**
 * Unit tests for the Main controller for the API.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class BlockchainTests {

    /**
     * The BlockchainService used to make requests to the API.
     */
    @Mock
    private BlockchainService blockchainService;

    /**
     * The BlockchainController used to make requests to the API.
     */
    @InjectMocks
    private BlockchainController blockchainController;

    /**
     * The RestTemplate used to make requests to the API.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and endpoints.
     */
    private static final String API_VERSION = "/api/v1";

    /**
     * The verify blockchain endpoint.
     */
    private static final String VERIFY_BLOCKCHAIN_ENDPOINT = "/blockchain/verifyBlockchain";

    /**
     * Tests the verifyBlockchain method.
     */
    @Test
    void testVerifyBlockchain() {
        blockchainService.createInitialBlockchain();
        ResponseEntity<Boolean> response = restTemplate.getForEntity(
                API_VERSION + VERIFY_BLOCKCHAIN_ENDPOINT, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the getBlockchain method.
     */
    @Test
    void testGetBlockchain() {
        // Arrange
        Block[] mockBlockchain = {};
        when(blockchainService.getBlockchain()).thenReturn(mockBlockchain);

        // Act
        ResponseEntity<Block[]> response = blockchainController.getBlockchain();

        // Assert
        assertEquals(200, response.getStatusCodeValue()); // checks if the status code is 200 OK
        assertArrayEquals(mockBlockchain, response.getBody()); // checks if the returned blockchain matches the mock
    }

    /**
     * Tests the getBlockById method.
     */
    @Test
    void testGetBlockById() {
        // Arrange
        Block mockBlock = blockchainService.getBlockById(0);
        when(blockchainService.getBlockById(0)).thenReturn(mockBlock);

        // Act
        ResponseEntity<Block> response = blockchainController.getBlockById(0);

        // Assert
        assertEquals(200, response.getStatusCodeValue()); // checks if the status code is 200 OK
        assertEquals(mockBlock, response.getBody()); // checks if the returned block matches the mock
    }
}
