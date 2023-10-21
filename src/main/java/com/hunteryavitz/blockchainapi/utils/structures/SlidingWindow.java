package com.hunteryavitz.blockchainapi.utils.structures;

import com.hunteryavitz.blockchainapi.entities.healthmetric.HealthMetricProductionModel;

import java.util.LinkedList;

/**
 * The SlidingWindow class is a data structure that holds a maximum number of elements.
 */
public class SlidingWindow {

    /**
     * The data structure that holds the elements.
     */
    private final LinkedList<IntPair> data;

    /**
     * The maximum number of elements that the data structure can hold.
     */
    private final int maxSize;

    /**
     * The IntPair class is a data structure that holds two integers.
     */
    static class IntPair {
        int first;
        int second;

        public IntPair(int first, int second) {
            this.first = first;
            this.second = second;
        }
    }

    /**
     * The SlidingWindow constructor.
     * @param maxSize the maximum number of elements that the data structure can hold
     */
    public SlidingWindow(int maxSize) {
        this.data = new LinkedList<>();
        this.maxSize = maxSize;
    }

    /**
     * The enqueueAndShift method adds an element to the data structure and shifts the elements to the right.
     * @param first the first integer
     * @param second the second integer
     */
    public void enqueueAndShift(int first, int second) {
        if (data.size() == maxSize) {
            data.removeLast();
        }
        data.addFirst(new IntPair(first, second));
    }

    /**
     * The isEmpty method checks if the data structure is empty.
     * @return true if the data structure is empty, false otherwise
     */
    public boolean isEmpty() {
        return data.isEmpty();
    }

    /**
     * The size method returns the size of the data structure.
     * @return the size of the data structure
     */
    public int size() {
        return data.size();
    }

    /**
     * The get method returns the element at the specified index and position.
     * @param index the index
     * @param position the position
     * @return the element at the specified index and position
     */
    public int get(int index, int position) {
        return (position == 0) ? data.get(index).first : data.get(index).second;
    }

    /**
     * The toString method returns a string representation of the data structure.
     * @return a string representation of the data structure
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (IntPair datum : data) {
            result.append(datum.first).append(" ").append(datum.second).append("\n");
        }
        return result.toString();
    }

    /**
     * The asJson method returns a json string representation of the data structure.
     * @return a json string representation of the data structure
     */
    public String asJson() {

        HealthMetricProductionModel healthMetricProductionModel = new HealthMetricProductionModel();
        int[] data1 = new int[12];
        int[] data2 = new int[12];
        for (int i = 0; i < data.size(); i++) {
            data1[i] = data.get(i).first;
            data2[i] = data.get(i).second;
        }
        healthMetricProductionModel.setDataset1(data1);
        healthMetricProductionModel.setDataset2(data2);
        return healthMetricProductionModel.asJson();
    }
}
