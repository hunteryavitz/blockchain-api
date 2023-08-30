package com.hunteryavitz.blockchainapi.entities;

import com.hunteryavitz.blockchainapi.constants.Status;
import lombok.Data;

@Data
public class Transaction {

    private long id;
    private String timestamp;
    private String source;
    private Status status;

    public Transaction(long id, String timestamp, String source, String status) {
        this.id = id;
        this.timestamp = timestamp;
        this.source = source;
        this.status = Enum.valueOf(Status.class, status);
    }
}
