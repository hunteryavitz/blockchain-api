package com.hunteryavitz.blockchainapi.entities;

import lombok.Data;

/**
 * The Block class represents a block in the blockchain.
 */
@Data
public class Block {

    /**
     * The index of the block in the blockchain.
     */
    private int index;

    /**
     * The hash of the previous block in the blockchain.
     */
    private String previous_hash;

    /**
     * The timestamp of the block.
     */
    private long timestamp;

    /**
     * The data of the block.
     */
    private String data;

    /**
     * The hash of the block.
     */
    private String hash;

    /**
     * The constructor for the Block class.
     */
    public Block() {}

    /**
     * The constructor for the Block class.
     * @param index The index of the block in the blockchain.
     * @param previous_hash The hash of the previous block in the blockchain.
     * @param timestamp The timestamp of the block.
     * @param data The data of the block.
     * @param hash The hash of the block.
     */
    public Block(int index, String previous_hash, long timestamp, String data, String hash) {
        this.index = index;
        this.previous_hash = previous_hash;
        this.timestamp = timestamp;
        this.data = data;
        this.hash = hash;
    }
}
