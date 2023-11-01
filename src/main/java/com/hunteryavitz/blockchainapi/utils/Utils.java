package com.hunteryavitz.blockchainapi.utils;

import com.hunteryavitz.blockchainapi.entities.Block;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * The Utils class is a utility class that provides methods for calculating
 */
public class Utils {

    /**
     * The calculateHash method calculates the hash of a block.
     * @param index The index of the block.
     * @param previous_hash The hash of the previous block.
     * @param timestamp The timestamp of the block.
     * @param data The data of the block.
     * @return The hash of the block.
     * @throws NoSuchAlgorithmException Thrown if the SHA-256 algorithm is not available.
     */
    public static String calculateHash(int index, String previous_hash, long timestamp, String data) throws NoSuchAlgorithmException {

        String value = index + previous_hash + timestamp + data;
        MessageDigest digest = MessageDigest.getInstance("SHA-256");

        byte[] hash = digest.digest(value.getBytes(StandardCharsets.UTF_8));
        final StringBuilder hexString = new StringBuilder();

        for (byte b : hash) {
            final String hex = Integer.toHexString(0xff & b);
            if (hex.length() == 1)
                hexString.append('0');
            hexString.append(hex);
        }

        return hexString.toString();
    }

    /**
     * The verifyBlockchain method verifies the integrity of the blockchain.
     * @param blockchain The blockchain to verify.
     * @return True if the blockchain is valid, false otherwise.
     */
    public static boolean verifyBlockchain(Block[] blockchain) {

        for (int i = 1; i < blockchain.length; i++) {
            Block currentBlock = blockchain[i];
            Block previousBlock = blockchain[i - 1];

            if (currentBlock == null) {
                return true;
            }

            try {
                if (!currentBlock.getHash().equals(
                        calculateHash(currentBlock.getIndex(), currentBlock.getPrevious_hash(),
                                currentBlock.getTimestamp(), currentBlock.getData()))) {
                    return false;
                }

                if (!currentBlock.getPrevious_hash().equals(previousBlock.getHash())) {
                    return false;
                }

            } catch (NoSuchAlgorithmException e) {
                throw new RuntimeException(e);
            }
        }

        return true;
    }

    /**
     * The getNextBlockIndexFromBlockchain method returns the index of the next block in the blockchain.
     * @param blockchain The blockchain to get the next block index from.
     * @return The index of the next block in the blockchain.
     */
    public static int getNextBlockIndexFromBlockchain(Block[] blockchain) {

        for (int i = 1; i < blockchain.length; i++) {
            if (blockchain[i] == null) {
                return i;
            }
        }

        return -1;
    }

    /**
     * The calculateLiveness method calculates the liveness of a node.
     * @param liveness The liveness of the node.
     * @return The liveness of the node.
     */
    public static int calculateLiveness(Boolean[] liveness) {

        int checksCount = 0;
        int checksPassed = 0;

        for (Boolean b : liveness) {
            if (b != null) {
                checksCount++;
                if (b) {
                    checksPassed++;
                }
            }
        }

        double livenessPercentage = (double) checksPassed / (double) checksCount;
        return (int) (livenessPercentage * 100);
    }

    /**
     * The updateLiveness method updates the liveness of a node.
     * @param liveness The liveness of the node.
     * @return The updated liveness of the node.
     */
    public static Boolean[] updateLiveness(Boolean[] liveness) {

        Boolean[] updatedLiveness = new Boolean[100];

        for (int i = 0; i < liveness.length; i++) {
            if (liveness[i] != null) {
                updatedLiveness[i] = liveness[i];
            } else {
                updatedLiveness[i] = Math.random() < 0.9;
                return updatedLiveness;
            }
        }

        return updatedLiveness;
    }

    /**
     * The readFileToString method reads a file to a string.
     * @param filePath The path of the file to read.
     * @return The string representation of the file.
     */
    public static String readFileToString(String filePath) {
        try {
            byte[] bytes = Files.readAllBytes(Paths.get(filePath));
            return new String(bytes);
        } catch (IOException e) {
            System.out.println("shit");
            return "";
        }
    }
}
