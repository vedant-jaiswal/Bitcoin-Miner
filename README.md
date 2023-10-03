# DOSP Project 1: Bitcoin Miner
----

## Author Details

Vedant Jaiswal
vedantjaiswal@ufl.edu

## Application Description

This application focuses on mining bitcoins with SHA 256 Hashes that begin with a specified number of leading zeros. The implementation is in Erlang, using the Actor Model. It employs a Client-Server architecture to simulate real-world usage.

## Design Details

- **Client-Server Architecture**:
  - Both client and server nodes are initialized on remote systems.
  - Remote nodes are connected using Erlang's Network Administration capabilities.
  - The client triggers the server to start mining with the desired number of leading zeros.

- **Actor Model Implementation**:
  - The server spawns Boss and Child Actors to handle the task.
  - Child Actors communicate with the Boss Actor through messages when valid bitcoins are mined.
  - This distributed approach significantly improves performance compared to a synchronous implementation.

- **Hash Generation and Checking**:
  - The input key is created by concatenating a random string with a group member's ID prefix.
  - The key is hashed to SHA 256 using Erlang's Crypto functions.
  - If the hash has the required leading zeros, it's considered a valid bitcoin and is outputted.

- **Performance Monitoring**:
  - Real times and CPU times are recorded per actor and aggregated.
  - Data is written to a text file for plotting and detailed analysis.

## Setup and Execution

1. **Setup**:
   - Client: `erl -name myclient@10.xx.xxx.xx -setcookie bitcoinminer`
   - Server: `erl -name myserver@10.xx.xxx.xx -setcookie bitcoinminer`

   (Replace `10.xx.xxx.xx` with the IPv4 address of the respective machines.)

2. **Connection**:
   - Client: `net_adm:ping('myserver@10.xx.xxx.xx').`

3. **Execution**:
   - Client Compilation: `c(miningclient).`
   - Server Compilation: `c(miningserver).`
   - Server Start: `miningserver:start_link().`
   - Client Call: `miningserver:triggerMiningOnServer(<<Number of leading zeros>>).`

## Testing Methodology

1. Initial testing for mining 512 coins synchronously (without actors).
2. Performance measurement for 4 leading zeros.
3. Implementation of the Actor Model.
4. Re-testing with the same parameters.
5. Significant performance improvement observed with the Actor Model.

**Machine Specifications**:
- Client: MacOS with 8 cores.
- Server: Windows with 8 cores.

(Note: Testing with 512 coins was chosen because it's a multiple of two, allowing testing with 2, 4, 8, 16, and 32 actors.)

## Testing Results

### Optimal Work Unit Size = 8 Actors

Results for mining 512 coins (4 leading zeros):

- 1 Actor (Synchronous):
  - CPU Time: 230,616 ms
  - Real Time: 218,125 ms

- 2 Actors:
  - CPU Time: 201,406 ms
  - Real Time: 110,122 ms

- 4 Actors:
  - CPU Time: 256,015 ms
  - Real Time: 70,260 ms

- 8 Actors (Best Performance):
  - CPU Time: 400,937 ms
  - Real Time: 62,650 ms

- 16 Actors:
  - CPU Time: 349,562 ms
  - Real Time: 50,785 ms

- 32 Actors:
  - CPU Time: 397,250 ms
  - Real Time: 57,192 ms

### Important Coin Details

- Leading Zeroes: 8 (Maximum)
  - ppasumarty;q/FNgETM: `00000000cdd88ee1821c4f97a51f82213b17c64e1962cea7562399439bba4204`

- Leading Zeroes: 7
  - ppasumarty;a0f1N2J/: `0000000b86b5ef0ba85669db8b43233b01da398e4f51b787dc1833f7e2ecbd02`
  - ppasumarty;7zd2xiel: `00000001e2841e0fe2814642192abe98bfa7fb8b24d76bc5db13be1aa38da48d`

(Notes: The system can mine coins with more leading zeros, but realistic timeframes limit the maximum to 8.)

### Maximum Number of Working Machines: 2

The Client-Server architecture is designed for a one-to-one relationship, limiting the maximum machines involved to 2.