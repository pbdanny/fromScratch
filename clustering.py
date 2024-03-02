"""
K Mean clustering from scratch
"""
import numpy as np
import matplotlib.pyplot as plt

class KMeans:
    def __init__(self, n_clusters, max_iters=100, random_state=None):
        self.n_clusters = n_clusters
        self.max_iters = max_iters
        self.random_state = random_state
        self.centroids = None

    def initialize_centroids(self, X):
        np.random.seed(self.random_state)
        indices = np.random.choice(len(X), self.n_clusters, replace=False)
        return X[indices]

    def assign_clusters(self, X):
        distances = np.linalg.norm(X[:, np.newaxis, :] - self.centroids, axis=2)
        return np.argmin(distances, axis=1)

    def update_centroids(self, X, labels):
        new_centroids = np.array([X[labels == i].mean(axis=0) for i in range(self.n_clusters)])
        return new_centroids

    def plot_iteration(self, X, labels, iteration):
        plt.scatter(X[:, 0], X[:, 1], c=labels, cmap='viridis', alpha=0.5)
        plt.scatter(self.centroids[:, 0], self.centroids[:, 1], c='red', marker='x', s=200, label='Centroids')
        plt.title(f'K-Means Clustering - Iteration {iteration}')
        plt.xlabel('Feature 1')
        plt.ylabel('Feature 2')
        plt.legend()
        plt.show()
        
    def fit(self, X):
        self.centroids = self.initialize_centroids(X)

        for i in range(self.max_iters):
            labels = self.assign_clusters(X)
            new_centroids = self.update_centroids(X, labels)

            # Check for convergence
            if np.allclose(new_centroids, self.centroids):
                break

            self.centroids = new_centroids

            # Plot the current iteration
            self.plot_iteration(X, labels, i + 1)
            
        return labels

# Helper part----
def euclidian_norm(X):
    """Same as np.linalg.norm
    """
    return np.sqrt(np.sum(np.power(X, 2)))



# Example usage:
if __name__ == "__main__":
    # Generate random data
    # np.random.seed(42)
    data = np.random.rand(50, 2)

    print("Data :")
    print(data)
    # Create KMeans instance and fit the data
    kmeans = KMeans(n_clusters=3, random_state=42)
    labels = kmeans.fit(data)

    # Print the final centroids and labels
    print("Final Centroids:")
    print(kmeans.centroids)
    print("Labels:")
    print(labels)
