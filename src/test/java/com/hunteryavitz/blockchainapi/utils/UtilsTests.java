package com.hunteryavitz.blockchainapi.utils;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.security.NoSuchAlgorithmException;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for the Utils class.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource(locations = "classpath:application-test.properties")
public class UtilsTests {

    /**
     * The testCalculateHash method tests the calculateHash method.
     * @throws NoSuchAlgorithmException if the hash algorithm is not found
     */
    @Test
    public void testCalculateHash() throws NoSuchAlgorithmException {

        int index = 1;
        String previousHash = "abc";
        long timestamp = 1234567890L;
        String data = "test data";

        String hash = Utils.calculateHash(index, previousHash, timestamp, data);

        assertNotNull(hash);
    }

    /**
     * The testVerifyBlockchain method tests the verifyBlockchain method.
     */
    @Test
    public void testVerifyBlockchain() {

        BlockchainService blockchainService = new BlockchainService();

        blockchainService.addBlockToBlockchain();
        Block[] blockchain = blockchainService.getBlockchain();

        boolean isValid = Utils.verifyBlockchain(blockchain);

        assertTrue(isValid);

        blockchain[1].setHash("abc");
        isValid = Utils.verifyBlockchain(blockchain);
        assertFalse(isValid);
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
