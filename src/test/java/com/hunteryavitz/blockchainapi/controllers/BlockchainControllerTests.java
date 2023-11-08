package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.ResponseEntity;

import java.util.Arrays;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

/**
 * Unit tests for the Blockchain controller.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class BlockchainControllerTests {

    /**
     * The BlockchainService used to provide setup tasks.
     */
    @Mock
    private BlockchainService blockchainService;

    /**
     * The RestTemplate used to make requests to the API.
     */
    @Autowired
    private TestRestTemplate restTemplate;

    /**
     * The API version and controller.
     */
    private static final String BLOCKCHAIN_PATH = "/api/v1/blockchain";

    /**
     * The verify blockchain endpoint.
     */
    private static final String VERIFY_BLOCKCHAIN_ENDPOINT = "/verifyBlockchain";

    /**
     * The get blockchain endpoint.
     */
    private static final String GET_BLOCKCHAIN_ENDPOINT = "/getBlockchain";

    /**
     * The get block by id endpoint.
     */
    private static final String GET_BLOCK_BY_ID_ENDPOINT = "/getBlockById";

    /**
     * The query string.
     */
    private static final String QUERY = "?";

    /**
     * The query parameter id.
     */
    private static final String QUERY_PARAM_ID = "id=0";

    /**
     * The ampersand.
     */
    private static final String AND = "&";

    /**
     * The query parameter test.
     */
    private static final String QUERY_PARAM_TEST = "test=true";

    /**
     * Tests the verifyBlockchain method returns true.
     */
    @Test
    void testVerifyBlockchain_whenBlockchainGood_returns200AndTrue() {
        blockchainService = new BlockchainService();
        blockchainService.createInitialBlockchain();

        ResponseEntity<Boolean> response = restTemplate.getForEntity(
                BLOCKCHAIN_PATH + VERIFY_BLOCKCHAIN_ENDPOINT, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.TRUE.equals(response.getBody()));
    }

    /**
     * Tests the verifyBlockchain method returns false.
     */
    @Test
    void testVerifyBlockchain_whenBlockchainBad_returns200AndFalse() {
        blockchainService = new BlockchainService();
        blockchainService.createInitialBlockchain();
        blockchainService.addMungedBlockToBlockchain();

        ResponseEntity<Boolean> response = restTemplate.getForEntity(
                BLOCKCHAIN_PATH + VERIFY_BLOCKCHAIN_ENDPOINT, Boolean.class);

        assert response.getStatusCode().is2xxSuccessful();
        assert (Boolean.FALSE.equals(response.getBody()));
    }

    /**
     * Tests the getBlockchain method returns blockchain.
     */
    @Test
    void testGetBlockchain_whenHasBlockchain_returns200AndBlockchain() {
        ResponseEntity<Block[]> response = restTemplate.getForEntity(
                BLOCKCHAIN_PATH + GET_BLOCKCHAIN_ENDPOINT, Block[].class);

        assert response.getStatusCode().is2xxSuccessful();
        assertNotNull(response.getBody());
    }

    /**
     * Tests the getBlockchain method returns empty.
     */
    @Test
    void testGetBlockchain_whenHasNoBlockchain_returns200AndEmptyArray() {
        when(blockchainService.getBlockchain()).thenReturn(new Block[0]);
        ResponseEntity<Block[]> response = restTemplate.getForEntity(
                BLOCKCHAIN_PATH
                        + GET_BLOCKCHAIN_ENDPOINT
                        + QUERY
                        + QUERY_PARAM_TEST, Block[].class);

        assert response.getStatusCode().is2xxSuccessful();
        assertEquals(0,
                Arrays.stream(Objects.requireNonNull(response.getBody())).count());
    }

    /**
     * Tests the getBlockById returns block.
     */
    @Test
    void testGetBlockById_whenIsBlock_returns200AndBlock() {
        ResponseEntity<Block> response = restTemplate.getForEntity(
                BLOCKCHAIN_PATH
                        + GET_BLOCK_BY_ID_ENDPOINT
                        + QUERY
                        + QUERY_PARAM_ID, Block.class);

        assertEquals(200, response.getStatusCode().value());
        assertEquals(0, Objects.requireNonNull(response.getBody()).getIndex());
    }

    /**
     * Tests the getBlockById returns null block.
     */
    @Test
    void testGetBlockById_whenNoBlock_returns200AndEmpty() {
        ResponseEntity<Block> response = restTemplate.getForEntity(
                BLOCKCHAIN_PATH
                        + GET_BLOCK_BY_ID_ENDPOINT
                        + QUERY
                        + QUERY_PARAM_ID
                        + AND
                        + QUERY_PARAM_TEST, Block.class);

        assertEquals(200, response.getStatusCode().value());
        assertNull(Objects.requireNonNull(response.getBody()).getHash());
    }
}
