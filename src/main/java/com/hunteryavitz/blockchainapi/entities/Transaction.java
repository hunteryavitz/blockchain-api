package com.hunteryavitz.blockchainapi.entities;

import com.hunteryavitz.blockchainapi.constants.Status;
import lombok.Data;

/**
 * The Transaction class represents a transaction in the blockchain.
 */
@Data
public class Transaction {

    /**
     * The id of the transaction.
     */
    private long id;

    /**
     * The timestamp of the transaction.
     */
    private String timestamp;

    /**
     * The source of the transaction.
     */
    private String source;

    /**
     * The status of the transaction.
     */
    private Status status;

    /**
     * The constructor for the Transaction class.
     * @param id The id of the transaction.
     * @param timestamp The timestamp of the transaction.
     * @param source The source of the transaction.
     * @param status The status of the transaction.
     */
    public Transaction(long id, String timestamp, String source, String status) {
        this.id = id;
        this.timestamp = timestamp;
        this.source = source;
        this.status = Enum.valueOf(Status.class, status);
    }
}
