package com.hunteryavitz.blockchainapi.utils;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import java.security.NoSuchAlgorithmException;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for the Utils class.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class UtilsTests {

    /**
     * The testCalculateHash method tests the calculateHash method.
     * @throws NoSuchAlgorithmException if the hash algorithm is not found
     */
    @Test
    public void testCalculateHash() throws NoSuchAlgorithmException {
        // Prepare test data
        int index = 1;
        String previousHash = "abc";
        long timestamp = 1234567890L;
        String data = "test data";

        // Call the method under test
        String hash = Utils.calculateHash(index, previousHash, timestamp, data);

        // Assert the result (this will depend on the actual hash algorithm)
        assertNotNull(hash);
        // Other assertions as needed...
    }

    /**
     * The testVerifyBlockchain method tests the verifyBlockchain method.
     */
    @Test
    public void testVerifyBlockchain() {
        BlockchainService blockchainService = new BlockchainService();

        // Prepare test data
        Block[] blockchain = blockchainService.getBlockchain();

        // Call the method under test
        boolean isValid = Utils.verifyBlockchain(blockchain);

        // Assert the result
        assertTrue(isValid);

        // More tests can be added for invalid blockchains
    }

    /**
     * The testGetNextBlockIndexFromBlockchain method tests the getNextBlockIndexFromBlockchain method.
     */
    @Test
    public void testGetNextBlockIndexFromBlockchain() {

        BlockchainService blockchainService = new BlockchainService();

        Block[] blockchain = blockchainService.getBlockchain();

        int nextIndex = Utils.getNextBlockIndexFromBlockchain(blockchain);
        int expectedNextIndex = -1;

        for (int i = 0; i < blockchain.length; i++) {
            if (blockchain[i] == null) {
                expectedNextIndex = i;
                break;
            }
        }

        assertEquals(expectedNextIndex, nextIndex);
    }

}
