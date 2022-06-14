using System;
using System.Collections;
using System.Collections.Generic;
// //By Necrolyte, https://github.com/Mustafa1177/JJ2ClientLib
using System.Net;
using System.Net.Sockets;
using System.Text;

namespace JJ2ClientLib.JJ2
{
    // The library
    public class JJ2Client
    {
        public object UserData { get; set; }

        private string _name = "(NoName)";
        private byte _char;
        private byte _team;
        private Socket Winsock1;
        private UdpClient Winsock2;
        private bool _connected = false;
        private string _serverAddress = "";
        private string _serverIPAddress = "";
        private ushort _serverPort = 10052;
        private IPEndPoint _remoteEP;
        private byte socketID = 0xFF;
        private byte _ID = 0xFF;
        private bool _packet0x0EWasSent = false;
        private byte _gameType;
        private int _maxScore;
        private const int _connectionlimit = 64;
        public bool[] ActiveClients = new bool[65];
        public JJ2.JJ2SocketInfo[] JJ2ClientsSockInfo = new JJ2.JJ2SocketInfo[65];
        public JJ2.JJ2Player[] Players = new JJ2.JJ2Player[255];
        public JJ2.JJ2Team[] Teams = new JJ2.JJ2Team[4];
        private bool _plusServer;
        private bool _idleServerMode;
        private bool _specialServer;
        private string _currentLevelName = "";
        private string _nextLevelName = "";
        private bool _cycling = false;
        private bool _isFirstScoreUpdate = true;
        private System.Timers.Timer every1SecTimer;
        private System.Timers.Timer AnimTimer;
        private bool udpTimerState = false;
        private byte ConnectionTimeOut = 30;
        private byte UDPCount = 0x3;

        // Options and extra values
        public string JJ2Version { get; set; } = "";
        public byte Plus { get; set; } = 1;
        public int PlusVersion { get; set; } = 0x50009;
        public byte _AutoSpec { get; set; } = 1;
        public short ExtraLatency { get; set; } = 0;
        public bool AntiSpam { get; set; } = false;

        private byte _antiSpamClearSeconds = 2;
        private ushort _antiSpamNumOfMsgsToKick = 20;
        private int _levelCRC32;
        private int _tilesetCRC32;

        public ulong TotalBytesRecv { get; set; } = 0UL;
        public ulong TotalBytesSent { get; set; } = 0UL;

        // JJ2+ Variables
        private byte _customGameMode = 0;

        public JJ2.JJ2PlusGameSettings PlusGameSettings { get; set; } = new JJ2.JJ2PlusGameSettings();

        public Dictionary<string, byte> scriptModules = new Dictionary<string, byte>(); // Name, Id
        public List<string> scriptsRequiredFiles = new List<string>();
        private int _serverPlusVersion = 0x0;
        private bool _scriptsEnabled = false;
        private bool _plusOnly;
        private JJ2.JJ2Plus_Game_State _gameState = (JJ2.JJ2Plus_Game_State)0;
        private bool _gameStarted = true;
        private bool _gameInProgress = false;
        private bool _isFirstGameState = true;

        public int TimeLimit { get; set; } = 0; // in ms
        public int TimeRemaining { get; set; } = 0; // in ms
        public DateTime timerInfoUpdateDate { get; set; }



        // ---------------------------- Client Events ----------------------------'
        public event Connected_EventEventHandler Connected_Event;

        public delegate void Connected_EventEventHandler(string serverIPAddrees, string serverAddress, ushort serverPort, object user);

        public event Failed_To_Connect_EventEventHandler Failed_To_Connect_Event;

        public delegate void Failed_To_Connect_EventEventHandler(string serverAddress, ushort serverPort, object user);

        public event Recveived_Server_Data_EventEventHandler Recveived_Server_Data_Event;

        public delegate void Recveived_Server_Data_EventEventHandler(byte socketIndex, byte PlayerID, string levelName, byte gameMode, byte maxScores, bool plusServer, object user);

        public event Joined_EventEventHandler Joined_Event;

        public delegate void Joined_EventEventHandler(byte socketIndex, byte PlayerID, string serverIPAddrees, string serverAddress, ushort serverPort, object user);

        public event Disconnected_EventEventHandler Disconnected_Event;

        public delegate void Disconnected_EventEventHandler(JJ2.JJ2_Disconnect_Message disconnectMessage, string serverIPAddrees, string serverAddress, ushort serverPort, object user);

        public event Client_Connected_EventEventHandler Client_Connected_Event;

        public delegate void Client_Connected_EventEventHandler(byte connectedSocketIndex, byte numberOfPlayersFromClient, object user);

        public event Client_Disconnected_EventEventHandler Client_Disconnected_Event;

        public delegate void Client_Disconnected_EventEventHandler(byte disconnectedSocketIndex, JJ2.JJ2_Disconnect_Message disconnectMessage, byte numberOfPlayersFromClient, object user);

        public event Players_List_Update_EventEventHandler Players_List_Update_Event;

        public delegate void Players_List_Update_EventEventHandler(byte[] updatedPlayersIDs, byte[] updatedClientsIndices, object user);

        public event Player_Joined_EventEventHandler Player_Joined_Event;

        public delegate void Player_Joined_EventEventHandler(string playerName, byte playerID, byte socketIndex, byte character, byte team, object user);

        public event Player_Left_EventEventHandler Player_Left_Event;

        public delegate void Player_Left_EventEventHandler(string playerName, JJ2.JJ2_Disconnect_Message disconnectMessage, byte playerID, byte socketIndex, object user);

        public event Message_Received_EventEventHandler Message_Received_Event;

        public delegate void Message_Received_EventEventHandler(string msg, string playerName, byte team, byte playerID, byte playerSocketIndex, object user);

        public event Console_Message_Recveived_EventEventHandler Console_Message_Recveived_Event;

        public delegate void Console_Message_Recveived_EventEventHandler(string msg, byte msgType, object user);

        public event Level_Initialized_EventEventHandler Level_Initialized_Event;

        public delegate void Level_Initialized_EventEventHandler(string levelName, string yourName, byte yourID, byte yourSocketIndex, object user);

        public event End_Of_Level_EventEventHandler End_Of_Level_Event;

        public delegate void End_Of_Level_EventEventHandler(byte winnerID, int winnerScore, byte[] playersIDs, byte[] playersPlaces, byte[] teamsIDs, byte[] teamsPlaces, object user);

        public event Idle_Server_Mode_Update_EventEventHandler Idle_Server_Mode_Update_Event;

        public delegate void Idle_Server_Mode_Update_EventEventHandler(bool idleServerModeState, object user);

        public event Latency_Update_EventEventHandler Latency_Update_Event;

        public delegate void Latency_Update_EventEventHandler(object user);

        public event Clients_State_Update_EventEventHandler Clients_State_Update_Event;

        public delegate void Clients_State_Update_EventEventHandler(object user);

        public event Client_Spectate_EventEventHandler Client_Spectate_Event;

        public delegate void Client_Spectate_EventEventHandler(bool spectatorMode, byte socketIndex, object user);

        public event Player_Spectate_EventEventHandler Player_Spectate_Event;

        public delegate void Player_Spectate_EventEventHandler(bool spectatorModeState, byte playerID, byte socketIndex, object user);

        public event Remote_Admins_Update_EventEventHandler Remote_Admins_Update_Event;

        public delegate void Remote_Admins_Update_EventEventHandler(object user);

        public event Game_State_Changed_EventEventHandler Game_State_Changed_Event;

        public delegate void Game_State_Changed_EventEventHandler(bool gameStarted, bool gameWasStarted, JJ2.JJ2Plus_Game_State newGameState, int timeRemaining, int timeLimit, bool newGame, bool firstTime, object user);

        public event JJ2_Plus_Network_Stream_Data_ArrivalEventHandler JJ2_Plus_Network_Stream_Data_Arrival;

        public delegate void JJ2_Plus_Network_Stream_Data_ArrivalEventHandler(byte[] packet, byte sourceId);

        public event Max_Resolution_Set_EventEventHandler Max_Resolution_Set_Event;

        public delegate void Max_Resolution_Set_EventEventHandler(ushort maxWidth, ushort maxHeight, object user);

        public event Team_State_Change_EventEventHandler Team_State_Change_Event;

        public delegate void Team_State_Change_EventEventHandler(byte team, bool enabled, object user);

        public event Game_Settings_Update_EventEventHandler Game_Settings_Update_Event;

        public delegate void Game_Settings_Update_EventEventHandler(JJ2.JJ2_Game_Type gameMode, JJ2.JJ2_Custom_Game_Type customGameMode, int maxScore, object user);

        public event Custom_IP_Update_EventEventHandler Custom_IP_Update_Event;

        public delegate void Custom_IP_Update_EventEventHandler(object user);

        public event Error_EventEventHandler Error_Event;

        public delegate void Error_EventEventHandler(bool disconnected, int errorCode, string errorMsg, object user);
        // Gameplay events [TCP]
        public event Gameplay_Player_Stats_List_Update_EventEventHandler Gameplay_Player_Stats_List_Update_Event;

        public delegate void Gameplay_Player_Stats_List_Update_EventEventHandler(object user);

        public event Gameplay_Teams_Scores_Set_EventEventHandler Gameplay_Teams_Scores_Set_Event;

        public delegate void Gameplay_Teams_Scores_Set_EventEventHandler(byte[] teamsUpdated, object user);

        public event Gameplay_Team_Score_Set_EventEventHandler Gameplay_Team_Score_Set_Event;

        public delegate void Gameplay_Team_Score_Set_EventEventHandler(byte team, int oldScore, int newScore, object user);

        public event Gameplay_Player_Deaths_Update_EventEventHandler Gameplay_Player_Deaths_Update_Event;

        public delegate void Gameplay_Player_Deaths_Update_EventEventHandler(byte playerID, int deaths, object user);
        // Gameplay events [UDP]
        public event Gameplay_Player_Roast_EventEventHandler Gameplay_Player_Roast_Event;

        public delegate void Gameplay_Player_Roast_EventEventHandler(byte victimID, int victimKills, byte killerID, int killerKills, object user);

        public event Gameplay_Player_Hit_EventEventHandler Gameplay_Player_Hit_Event;

        public delegate void Gameplay_Player_Hit_EventEventHandler(byte victimID, byte victimHealth, byte attackerID, object user);

        public event Gameplay_Teams_Scores_Update_EventEventHandler Gameplay_Teams_Scores_Update_Event;

        public delegate void Gameplay_Teams_Scores_Update_EventEventHandler(byte[] teamsUpdated, object user);

        public event Gameplay_Team_Score_Update_EventEventHandler Gameplay_Team_Score_Update_Event;

        public delegate void Gameplay_Team_Score_Update_EventEventHandler(byte team, int oldScore, int newScore, object user);

        public event Gameplay_Team_Scored_EventEventHandler Gameplay_Team_Scored_Event;

        public delegate void Gameplay_Team_Scored_EventEventHandler(byte team, int oldScore, int newScore, object user);

        public event Gameplay_CTF_Flags_Update_EventEventHandler Gameplay_CTF_Flags_Update_Event;

        public delegate void Gameplay_CTF_Flags_Update_EventEventHandler(byte[] teamFlagsUpdated, object user);

        public event Gameplay_CTF_Flag_Update_EventEventHandler Gameplay_CTF_Flag_Update_Event;

        public delegate void Gameplay_CTF_Flag_Update_EventEventHandler(byte team, bool flagIsCaptured, byte carrierID, object user);

        public event Gameplay_Plus_Bullet_Shoot_EventEventHandler Gameplay_Plus_Bullet_Shoot_Event;

        public delegate void Gameplay_Plus_Bullet_Shoot_EventEventHandler(byte gun, bool power, short bulletX, short bulletY, sbyte bulletVx, sbyte bulletVy, sbyte bulletAngle, sbyte playerVx, byte lifetime, byte ammoRemaining, sbyte direction, bool vertical, byte playerID, bool friendly, object user);


        // ---------------------------- Client Functions ----------------------------'
        public string JoinServer(string serverAddress, object user, string name = "", ushort port = 10052)
        {
            // Check if arguments are valid
            if (name.Length > 20)
            {
                return "Invalid name length.";
            }

            if (port == 0)
            {
                return "Invalid port number.";
            }

            // Store given args
            UserData = user;
            if (!string.IsNullOrEmpty(name))
            {
                _name = name;
            }

            _serverAddress = serverAddress;
            _serverPort = port;
            _connected = false;

            // Init sockets
            if (Winsock1 is object)
            {
                Winsock1.Close();
            }

            Winsock1 = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            if (Winsock2 is null)
            {
                Winsock2 = new UdpClient(0);
            }

            // initialize timers
            every1SecTimer.Elapsed -= every1SecTimerTick;
            every1SecTimer.Stop();
            every1SecTimer.Elapsed += every1SecTimerTick;
            every1SecTimer.Interval = 1000d;
            every1SecTimer.Start();
            AnimTimer.Elapsed -= AnimTimerTick;
            AnimTimer.Stop();
            AnimTimer.Elapsed += AnimTimerTick;
            AnimTimer.Interval = 125d;
            // AnimTimer.Start()

            // Connect
            Winsock1.BeginConnect(serverAddress, port, new AsyncCallback(Winsock1_Connect_Event), null);
            return "";
        }

        public string Leave()
        {
            WinsockClose();
            return default;
        }

        public JJ2Client()
        {
            every1SecTimer = new System.Timers.Timer();
            AnimTimer = new System.Timers.Timer();
            for (int i = 0; i <= 32 - 1; i++)
                Players[i] = new JJ2.JJ2Player(0xFF, 0, 0, null);
            for (int i = 0, loopTo = Teams.Length - 1; i <= loopTo; i++)
                Teams[i] = new JJ2.JJ2Team() { Color = (JJ2.JJ2_Player_Team)i, FlagCarriedByPlayerID = 0xFF };
        }

        public void Dispose()
        {
            every1SecTimer.Elapsed -= every1SecTimerTick;
            every1SecTimer.Dispose();
            AnimTimer.Dispose();
            if (Winsock1 is object)
            {
                Winsock1.Close();
            }

            if (Winsock2 is object)
            {
                Winsock2.Close();
            }
        }

        private byte[] IdleServerPacket = new[] { (byte)0x42, (byte)0xC0, (byte)0x2, (byte)0x46, (byte)0x2, (byte)0x27, (byte)0x0, (byte)0xCC };
        private byte[] _joiningData1 = new[] { (byte)0x9, (byte)0xF, (byte)0x0, (byte)0x0, (byte)0x32, (byte)0x34, (byte)0x20, (byte)0x20, (byte)0x1 };

        private void Winsock1_Connect_Event(IAsyncResult ar) // onConnect
        {
            try
            {
                Winsock1.EndConnect(ar);
                _connected = true;
                socketID = 0xFF;
                _idleServerMode = false;
                _plusServer = Conversions.ToBoolean(Plus);
                _serverPlusVersion = 0;
                PlusGameSettings = new JJ2.JJ2PlusGameSettings();
                _packet0x0EWasSent = false;
                var ipPort = Winsock1.RemoteEndPoint.ToString().Split(':');
                _remoteEP = new IPEndPoint(IPAddress.Parse(ipPort[0]), Conversions.ToInteger(ipPort[1]));
                _serverIPAddress = ipPort[0];
                if (Winsock2 is null)
                {
                    Winsock2 = new UdpClient(0);
                }

                Reset();
                Connected_Event?.Invoke(ipPort[0], _serverAddress, Conversions.ToUShort(ipPort[1]), UserData);
                Array.Copy(BitConverter.GetBytes(Conversions.ToUShort(Winsock2.Client.LocalEndPoint.ToString().Split(':')[1])), 0, _joiningData1, 2, 2);
                // assign local port of UDP socket

                TotalBytesRecv = 0UL;
                TotalBytesSent = 0UL;
                EndPoint argremoteEP = _remoteEP;
                Winsock2.Client.BeginReceiveFrom(BufferUDP, 0, BufferUDP.Length, SocketFlags.None, ref argremoteEP, new AsyncCallback(Winsock2_DataArrival), _remoteEP);
                Winsock1.BeginReceive(BufferTCP, 0, BufferTCP.Length, SocketFlags.None, new AsyncCallback(Winsock1_DataArrival), null);
                if (!string.IsNullOrEmpty(JJ2Version))
                {
                    var versionBytes = Encoding.ASCII.GetBytes(JJ2Version);
                    if (versionBytes.Length <= 4)
                    {
                        Array.Copy(versionBytes, 0, _joiningData1, 4, versionBytes.Length);
                    }
                }

                Winsock1SendData(_joiningData1);
            }
            catch (ArgumentException argEx)
            {
            }
            catch (ObjectDisposedException obDisEx)
            {
            }
            catch (SocketException sockEx)
            {
                WinsockClose(255);
            }
        }

        private void WinsockClose(byte disconType = 0)
        {
            if (Winsock1 is object)
            {
                Winsock1.Close();
            }

            if (_connected)
            {
                _connected = false;
                _packet0x0EWasSent = false;
                if (disconType != 0xFF)
                {
                    Disconnected_Event?.Invoke((JJ2.JJ2_Disconnect_Message)disconType, _serverIPAddress, _serverAddress, _serverPort, UserData);
                }

                _serverIPAddress = "";
            }

            if (disconType == 0xFF)
            {
                Failed_To_Connect_Event?.Invoke(_serverAddress, _serverPort, UserData);
            }
        }

        private void Reset()
        {
            for (byte i = 0; i <= _connectionlimit; i++)
            {
                ActiveClients[i] = false;
                if (JJ2ClientsSockInfo[i] is object)
                {
                    JJ2ClientsSockInfo[i].reset();
                }
            }

            for (byte i2 = 0, loopTo = (byte)(Players.Length - 1); i2 <= loopTo; i2++)
            {
                if (Players[i2] is object)
                {
                    Players[i2].reset(PlusServer);
                }
            }

            for (int i = 0, loopTo1 = Teams.Length - 1; i <= loopTo1; i++)
            {
                Teams[i].Enabled = false;
                Teams[i].Reset();
            }

            Teams[0].Enabled = true;
            Teams[1].Enabled = true;
            PlusGameSettings = new JJ2.JJ2PlusGameSettings();
            _plusOnly = false;
            _serverPlusVersion = 0;
            _scriptsEnabled = false;
            scriptsRequiredFiles.Clear();
            scriptModules.Clear();
            _isFirstGameState = true;
            _gameInProgress = false;
            _specialServer = false;
            _ID = 0xFF;
            socketID = 0xFF;
        }

        private byte[] PlusCheck = new byte[4];
        private byte[] CheckDatafrom10for9 = new byte[4];
        private byte[] joiningData2Plus = new[] { (byte)0x8, (byte)0x3F, (byte)0x20, (byte)0x3, (byte)0x9, (byte)0x0, (byte)0x5, (byte)0x0 };

        public Encoding DefaultEncoding { get; set; } = Encoding.Default;
        public Encoding DefaultNameEncoding { get; set; } = Encoding.ASCII;

        private byte[] BufferTCP = new byte[10240];

        private void Winsock1_DataArrival(IAsyncResult ar)
        {
            try
            {
                int dataLength = Winsock1.EndReceive(ar);
                var recv = new byte[dataLength];
                Array.Copy(BufferTCP, recv, dataLength);
                if (_plusServer)
                {
                    Winsock1_DataArrival_Read_PLUS(recv);
                }
                else
                {
                    Winsock1_DataArrival_Read(recv);
                }

                TotalBytesRecv = (ulong)Math.Round(TotalBytesRecv + (decimal)dataLength);
                Winsock1.BeginReceive(BufferTCP, 0, BufferTCP.Length, SocketFlags.None, new AsyncCallback(Winsock1_DataArrival), null);
            }
            catch (NullReferenceException nullEx)
            {
            }
            catch (ObjectDisposedException obDisEx)
            {
            }
            catch (ArgumentException argEx)
            {
            }
            // WinsockClose(&H0)'''''''''''''
            catch (SocketException sockEx)
            {
                WinsockClose(0x7);
            }
        }

        private void Winsock1_DataArrival_Read_PLUS(byte[] recv)
        {
            // '''''''Try
            if (recv.Length > 2)
            {
                uint packetLength = 0U;
                uint packetRealLength = 0U;
                var packetContentLength = default(uint); // exclude length byte(packet length from packetID to the end)
                uint Starting = 0U;
                int addnmbr = 0;
                byte packetID = 0x0;
                int packetNum = 0;
                bool readingComplete = false;
                int packStartingIndex = 0;
                while (readingComplete == false)
                {
                    // If recv.Length > Starting + addnmbr + 1 Then '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                    if (!(Starting + 0L < recv.Length)) // new thing prevents OutOfRange reading
                    {
                        break;
                    }

                    if (recv[(int)(Starting + 0L)] == 0)
                    {
                        if (Starting + 1L == recv.Length)
                        {
                            break;
                        }
                        else if (Starting + 1L < recv.Length)
                        {
                            if (recv[(int)(Starting + 1L)] == 0)
                            {
                                break;
                            }
                        }

                        var packetLengthInBytes = new[] { recv[(int)(Starting + 1L)], recv[(int)(Starting + 2L)] };
                        packetLength = BitConverter.ToUInt16(packetLengthInBytes, 0);
                        packetRealLength = (uint)(packetLength + 3L);
                        packetContentLength = packetLength;
                        addnmbr = 2;
                        if (packetRealLength > recv.Length)
                        {
                            break;
                        }
                    }

                    if (recv[(int)(Starting + 0L)] != 0)
                    {
                        if (recv[(int)Starting] > recv.Length - Starting)
                        {
                            break;
                        }

                        packetLength = recv[(int)(Starting + 0L)];
                        packetRealLength = packetLength;
                        packetContentLength = (uint)(packetLength - 1L);
                        addnmbr = 0;
                    }

                    packStartingIndex = (int)(Starting + addnmbr);
                    packetID = recv[packStartingIndex + 1];


                    // ''''''''''''

                    switch (packetID)
                    {
                        case 0x1B: // Chat message
                            {
                                byte senderSocketID = recv[packStartingIndex + 2];
                                byte team = recv[packStartingIndex + 3];
                                string message = Encoding.Default.GetString(recv, packStartingIndex + 4, (int)(packetLength - 3L));
                                byte senderPlayerID = JJ2ClientsSockInfo[senderSocketID].PlayerID[0];
                                if (senderPlayerID != 0xFF)
                                {
                                    Message_Received_Event?.Invoke(message, Players[(int)senderPlayerID].Name, team, senderPlayerID, senderSocketID, UserData);
                                }

                                // Anti-spam system
                                if (AntiSpam != false)
                                {
                                    if (senderSocketID < JJ2ClientsSockInfo.Length & JJ2ClientsSockInfo[senderSocketID] is object)
                                    {
                                        if (JJ2ClientsSockInfo[senderSocketID].AntiSpamCount < ushort.MaxValue)
                                        {
                                            JJ2ClientsSockInfo[senderSocketID].AntiSpamCount = (ushort)(JJ2ClientsSockInfo[senderSocketID].AntiSpamCount + 1);
                                        }

                                        if (JJ2ClientsSockInfo[senderSocketID].AntiSpamCount >= _antiSpamNumOfMsgsToKick)
                                        {
                                            this.SendMessage("/kick " + ((int)JJ2ClientsSockInfo[(int)senderSocketID].PlayerID[0] + 1) + " |(auto) |fuck off");
                                            JJ2ClientsSockInfo[senderSocketID].AntiSpamCount = 0;
                                        }
                                    }
                                }

                                break;
                            }

                        case 0xD: // Player left
                            {
                                byte disconnectMsg = recv[packStartingIndex + 2];
                                byte leftClientSocketIndex = recv[packStartingIndex + 3];
                                if (leftClientSocketIndex != socketID)
                                {
                                    if (leftClientSocketIndex < JJ2ClientsSockInfo.Length)
                                    {
                                        ActiveClients[leftClientSocketIndex] = false;
                                        if (JJ2ClientsSockInfo[leftClientSocketIndex] is object)
                                        {
                                            if (Conversions.ToBoolean(JJ2ClientsSockInfo[leftClientSocketIndex].NumOfPlayers))
                                            {
                                                Client_Disconnected_Event?.Invoke(leftClientSocketIndex, (JJ2.JJ2_Disconnect_Message)disconnectMsg, JJ2ClientsSockInfo[(int)leftClientSocketIndex].NumOfPlayers, UserData);
                                                for (byte wokingOnPlayer = 0, loopTo = (byte)((int)JJ2ClientsSockInfo[leftClientSocketIndex].NumOfPlayers - 1); wokingOnPlayer <= loopTo; wokingOnPlayer++)
                                                {
                                                    if ((int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer] != 0xFF)
                                                    {
                                                        if (Players[(int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer]] is object)
                                                        {
                                                            if ((int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer] != 0xFF & (int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer] < Players.Length)
                                                            {
                                                                string playerName = Players[(int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer]].Name;
                                                                byte playerID = JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer];
                                                                JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer] = 0xFF;
                                                                if ((int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer] != 0xFF)
                                                                {
                                                                    Players[(int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer]].reset();
                                                                }

                                                                Player_Left_Event?.Invoke(playerName, (JJ2.JJ2_Disconnect_Message)disconnectMsg, playerID, leftClientSocketIndex, UserData);
                                                            }
                                                        }
                                                    }
                                                }

                                                JJ2ClientsSockInfo[leftClientSocketIndex].reset();
                                            }
                                        }
                                    }
                                }
                                else
                                {
                                    // You 
                                    WinsockClose(disconnectMsg);
                                }

                                break;
                            }

                        case 0x10: // Info
                            {
                                socketID = recv[packStartingIndex + 2];
                                byte levelNameLength = recv[packStartingIndex + 4];
                                socketID = recv[packStartingIndex + 2];
                                _ID = recv[packStartingIndex + 3];
                                _nextLevelName = Encoding.ASCII.GetString(recv).Substring(packStartingIndex + 5, levelNameLength);
                                _gameType = recv[packStartingIndex + levelNameLength + 13];
                                _maxScore = recv[packStartingIndex + levelNameLength + 14];
                                if (recv[packStartingIndex] <= 15 + levelNameLength)
                                {
                                    // Non-plus server
                                    _plusServer = false;
                                    AnimTimer.Start();
                                    socketID = recv[(int)(Starting + addnmbr + 2L)];
                                    var joiningData2Part1 = new[] { (byte)0, (byte)0xE, (byte)0x1, (byte)0x1, (byte)0x1, (byte)0x18, (byte)0x20, (byte)0x28, (byte)0x11 }; // {&H19, &HE, &H1, &H1, &H0, &H1, &H10, &H18, &H20, &H28, &H11, &H1, &HA, &HD, &H0, &H0, &H52, &H65, &H63, &H6F, &H72, &H64, &H65, &H72, &H0}
                                    var joiningData2Part2 = DefaultNameEncoding.GetBytes(_name);
                                    var joiningData2 = new byte[joiningData2Part1.Length + joiningData2Part2.Length + 1];
                                    Array.Copy(joiningData2Part1, joiningData2, joiningData2Part1.Length);
                                    Array.Copy(joiningData2Part2, 0, joiningData2, joiningData2Part1.Length, joiningData2Part2.Length);
                                    joiningData2[0] = (byte)joiningData2.Length;
                                    if (!_packet0x0EWasSent)
                                    {
                                        _packet0x0EWasSent = true;
                                        if (Plus != 0)
                                        {
                                            Winsock1SendData(joiningData2Plus);
                                        }

                                        Winsock1SendData(joiningData2);
                                    }
                                }
                                else
                                {
                                    // Plus server
                                    _plusServer = true;
                                    AnimTimer.Stop();
                                    var joiningData3PlusPart1 = new[] { (byte)0x19, (byte)0xE, (byte)0x1, (byte)0x1, (byte)0, (byte)0x1, (byte)0x10, (byte)0x18, (byte)0x20, (byte)0x28, (byte)0x11, (byte)0x1, (byte)0xA, (byte)0xD, (byte)0x0, (byte)0x0 }; // {&H19, &HE, &H1, &H1, &H0, &H1, &H10, &H18, &H20, &H28, &H11, &H1, &HA, &HD, &H0, &H0, &H52, &H65, &H63, &H6F, &H72, &H64, &H65, &H72, &H0}
                                    var joiningData3PlusPart2 = DefaultNameEncoding.GetBytes(_name);
                                    var joiningData3Plus = new byte[joiningData3PlusPart1.Length + joiningData3PlusPart2.Length + 1];
                                    Array.Copy(joiningData3PlusPart1, joiningData3Plus, joiningData3PlusPart1.Length);
                                    Array.Copy(joiningData3PlusPart2, 0, joiningData3Plus, joiningData3PlusPart1.Length, joiningData3PlusPart2.Length);
                                    joiningData3Plus[0] = (byte)joiningData3Plus.Length;
                                    _nextLevelName = Encoding.ASCII.GetString(recv).Substring(packStartingIndex + 5, levelNameLength);
                                    PlusCheck[0] = recv[packStartingIndex + 16 + levelNameLength - 1];
                                    PlusCheck[1] = recv[packStartingIndex + 16 + levelNameLength];
                                    PlusCheck[2] = recv[packStartingIndex + 16 + levelNameLength + 1];
                                    PlusCheck[3] = recv[packStartingIndex + 16 + levelNameLength + 2];
                                    CheckDatafrom10for9[0] = recv[packStartingIndex + 20 + levelNameLength - 1];
                                    CheckDatafrom10for9[1] = recv[packStartingIndex + 20 + levelNameLength];
                                    CheckDatafrom10for9[2] = recv[packStartingIndex + 20 + levelNameLength + 1];
                                    CheckDatafrom10for9[3] = recv[packStartingIndex + 20 + levelNameLength + 2];
                                    var UDPPacket = new[] { (byte)0xA, (byte)0x15, (byte)0x9, (byte)0x0 };
                                    Winsock2SendData(UDPPacket);
                                    Array.Copy(BitConverter.GetBytes(PlusVersion), 0, joiningData2Plus, 4, 4); // Write your plus version to packet
                                    Winsock1SendData(joiningData2Plus);
                                    if (!_packet0x0EWasSent)
                                    {
                                        _packet0x0EWasSent = true;
                                        Winsock1SendData(joiningData3Plus);
                                    }
                                }

                                Recveived_Server_Data_Event?.Invoke(socketID, _ID, _currentLevelName, _gameType, (byte)_maxScore, _plusServer, UserData);
                                break;
                            }

                        case 0x11: // Player joined
                            {
                                byte joinedClientSocketIndex = recv[packStartingIndex + 2];
                                byte numIfJoinedPlayers = recv[packStartingIndex + 3];
                                ushort playerArrStartingIndex = (ushort)(packStartingIndex + 4);
                                if (joinedClientSocketIndex < _connectionlimit)
                                {
                                    // init socket info
                                    if (JJ2ClientsSockInfo[joinedClientSocketIndex] is null)
                                    {
                                        JJ2ClientsSockInfo[joinedClientSocketIndex] = new JJ2.JJ2SocketInfo();
                                    }
                                    else
                                    {
                                        JJ2ClientsSockInfo[joinedClientSocketIndex].reset();
                                    }

                                    for (byte workingOnPlayer = 0, loopTo1 = (byte)(numIfJoinedPlayers - 1); workingOnPlayer <= loopTo1; workingOnPlayer++)
                                    {
                                        byte playerNumber = recv[playerArrStartingIndex];
                                        byte charTeam = recv[playerArrStartingIndex + 1];
                                        if (workingOnPlayer >= JJ2ClientsSockInfo[joinedClientSocketIndex].PlayerID.Length)
                                        {
                                            break; // Very important, this might causes problems in players of server!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        }

                                        if (joinedClientSocketIndex < JJ2ClientsSockInfo.Length & playerNumber < Players.Length)
                                        {
                                            JJ2ClientsSockInfo[joinedClientSocketIndex].PlayerID[workingOnPlayer] = playerNumber;
                                            byte fdgfdgdfgdfgfdg = JJ2ClientsSockInfo[joinedClientSocketIndex].PlayerID[workingOnPlayer];

                                            // init player
                                            if (Players[playerNumber] is null)
                                            {
                                                Players[playerNumber] = new JJ2.JJ2Player(joinedClientSocketIndex, (byte)(charTeam % 0x10), (byte)Math.Round(Math.Floor(charTeam / 16d)), JJ2ClientsSockInfo[joinedClientSocketIndex]);
                                            }
                                            else
                                            {
                                                Players[playerNumber].reset();
                                                Players[playerNumber].Update(joinedClientSocketIndex, (byte)(charTeam % 0x10), (byte)Math.Round(Math.Floor(charTeam / 16d)), JJ2ClientsSockInfo[joinedClientSocketIndex]);
                                                JJ2ClientsSockInfo[joinedClientSocketIndex].Active = false;
                                            } // make it true whenever u recv udp packet 0x02 or until tcp packet 0x44 tells u to do so.

                                            Array.Copy(recv, (int)playerArrStartingIndex + 3, Players[(int)playerNumber].Color, 0, 4);
                                            byte playerNameLength = 0;
                                            ushort whileHelper = (ushort)(playerArrStartingIndex + 13);
                                            while (recv[whileHelper] != 0x0)
                                            {
                                                playerNameLength = (byte)(playerNameLength + 1);
                                                whileHelper = (ushort)(whileHelper + 1);
                                                if (recv.Length <= whileHelper)
                                                {
                                                    break;
                                                }
                                            }

                                            Players[playerNumber].Name = Encoding.UTF7.GetString(recv, playerArrStartingIndex + 13, playerNameLength);
                                            JJ2ClientsSockInfo[joinedClientSocketIndex].NumOfPlayers = (byte)(JJ2ClientsSockInfo[joinedClientSocketIndex].NumOfPlayers + 1);
                                            playerArrStartingIndex = (ushort)(playerArrStartingIndex + (14 + playerNameLength));
                                            Player_Joined_Event?.Invoke(Players[(int)playerNumber].Name, playerNumber, joinedClientSocketIndex, (byte)Players[(int)playerNumber].Character, Players[(int)playerNumber].Team, UserData);
                                        }
                                    }

                                    ActiveClients[joinedClientSocketIndex] = true;
                                    Client_Connected_Event?.Invoke(joinedClientSocketIndex, JJ2ClientsSockInfo[(int)joinedClientSocketIndex].NumOfPlayers, UserData);
                                }

                                break;
                            }

                        case 0x12: // Player list update
                            {
                                byte totNumOfPlayers = recv[packStartingIndex + 2];
                                var updatedClientsIndices = new List<byte>();
                                var updatedPlayersIDs = new List<byte>();
                                var numOfPlayersDoneWithInSock = new byte[65];
                                if (totNumOfPlayers != 0)
                                {
                                    short playerArrStartingIndex = (short)(packStartingIndex + 3);
                                    for (byte workingOnPlayer = 0, loopTo2 = (byte)(totNumOfPlayers - 1); workingOnPlayer <= loopTo2; workingOnPlayer++)
                                    {
                                        byte playerSocketIndex = recv[playerArrStartingIndex];
                                        byte playerNumber = recv[playerArrStartingIndex + 1];
                                        byte charTeam = recv[playerArrStartingIndex + 2];
                                        if (playerSocketIndex < JJ2ClientsSockInfo.Length & playerNumber < Players.Length)
                                        {
                                            updatedClientsIndices.Add(playerSocketIndex);
                                            updatedPlayersIDs.Add(playerNumber);

                                            // init socket info
                                            if (JJ2ClientsSockInfo[playerSocketIndex] is null)
                                            {
                                                JJ2ClientsSockInfo[playerSocketIndex] = new JJ2.JJ2SocketInfo();
                                            }
                                            else
                                            {
                                                // JJ2ClientsSockInfo(playerSocketIndex).reset()
                                            }

                                            JJ2ClientsSockInfo[playerSocketIndex].PlayerID[numOfPlayersDoneWithInSock[playerSocketIndex]] = playerNumber;
                                            numOfPlayersDoneWithInSock[playerSocketIndex] = (byte)(numOfPlayersDoneWithInSock[playerSocketIndex] + 1);
                                            ActiveClients[playerSocketIndex] = true;

                                            // init player
                                            if (Players[playerNumber] is null)
                                            {
                                                Players[playerNumber] = new JJ2.JJ2Player(playerSocketIndex, (byte)(charTeam % 0x10), recv[playerArrStartingIndex + 3], JJ2ClientsSockInfo[playerSocketIndex]);
                                            }
                                            else
                                            {
                                                // Players(playerNumber).reset()
                                                Players[playerNumber].Update(playerSocketIndex, (byte)(charTeam % 0x10), recv[playerArrStartingIndex + 3], JJ2ClientsSockInfo[playerSocketIndex]);
                                            }

                                            Players[playerNumber].ClearStats(_plusServer);
                                            Array.Copy(recv, (int)playerArrStartingIndex + 4, Players[(int)playerNumber].Color, 0, 4);
                                            byte playerNameLength = 0;
                                            ushort whileHelper = (ushort)(playerArrStartingIndex + 14);
                                            while (recv[whileHelper] != 0x0)
                                            {
                                                playerNameLength = (byte)(playerNameLength + 1);
                                                whileHelper = (ushort)(whileHelper + 1);
                                                if (recv.Length == whileHelper)
                                                {
                                                    break;
                                                }
                                            }

                                            Players[playerNumber].Name = Encoding.UTF7.GetString(recv, playerArrStartingIndex + 14, playerNameLength);

                                            // assign NumOfPlayers
                                            byte tempNumOfPlayersFromClient = 0;
                                            foreach (byte b in JJ2ClientsSockInfo[playerSocketIndex].PlayerID)
                                            {
                                                if (b != 0xFF)
                                                {
                                                    tempNumOfPlayersFromClient = (byte)(tempNumOfPlayersFromClient + 1);
                                                }
                                            }

                                            JJ2ClientsSockInfo[playerSocketIndex].NumOfPlayers = tempNumOfPlayersFromClient;
                                            playerArrStartingIndex = (short)(playerArrStartingIndex + (15 + playerNameLength));
                                            // MsgBox(Players(playerNumber).Name & " in server")
                                        }
                                    }
                                }

                                Players_List_Update_Event?.Invoke(updatedPlayersIDs.ToArray(), updatedClientsIndices.ToArray(), UserData);
                                break;
                            }

                        case 0x13:
                            {
                                ChangeLevel(_nextLevelName, 0);
                                var UDPPacket9_2 = new[] { (byte)0x0, (byte)0x0, (byte)0x9, (byte)0xC0, CheckDatafrom10for9[0], CheckDatafrom10for9[1], CheckDatafrom10for9[2], CheckDatafrom10for9[3] };
                                Winsock2SendData(UDPPacket9_2);
                                udpTimerState = true;
                                if (_ID != 0xFF)
                                {
                                    if (_AutoSpec != 0)
                                    {
                                        var spectating = new[] { (byte)0x3, (byte)0x42, (byte)0x21 };
                                        Winsock1SendData(spectating);
                                    }

                                    Joined_Event?.Invoke(socketID, _ID, _serverIPAddress, _serverAddress, _serverPort, UserData);
                                }

                                break;
                            }

                        case 0x16:
                            {
                                if (recv.Length - packStartingIndex > 11)
                                {
                                    int levelNameLength = recv[packStartingIndex + 10] - 1;
                                    if (recv.Length - packStartingIndex >= 11 + levelNameLength) // recv(10)=lvlNameLeng
                                    {
                                        _nextLevelName = Encoding.ASCII.GetString(recv, packStartingIndex + 11, levelNameLength);
                                        int levelCRC32 = BitConverter.ToInt32(recv, packStartingIndex + 11 + levelNameLength + 1);
                                        PlusCheck[0] = recv[packStartingIndex + 11 + levelNameLength + 1];
                                        PlusCheck[1] = recv[packStartingIndex + 11 + levelNameLength + 2];
                                        PlusCheck[2] = recv[packStartingIndex + 11 + levelNameLength + 3];
                                        PlusCheck[3] = recv[packStartingIndex + 11 + levelNameLength + 4];
                                        ChangeLevel(_nextLevelName, levelCRC32);
                                        udpTimerState = true;
                                    }
                                }

                                break;
                            }

                        case 0x17:
                            {
                                // Battle: 0E 17 01 05 04 01 05 02 00 03 02 04 03 05 
                                // TB: 17 17 01 04 02 01 01 02 00 03 03 04 05 00 03 01 03 02 02 04 01 05 03 
                                // '''_gameInProgress = False
                                _cycling = true;
                                int winnerID = 0xFF; // Applicable for vanilla JJ2
                                int winnerScore = -1; // Applicable for vanilla JJ2
                                int currIndex = packStartingIndex + 3;
                                byte numOfTeams;
                                byte[] teamsIDs;
                                byte[] teamsPlaces;

                                // Determine the number of teams in packet
                                if (GameType == JJ2.JJ2_Game_Type.CAPTURE)
                                {
                                    numOfTeams = recv[currIndex];
                                    currIndex += 1; // point to the first array
                                }
                                else // teams are disabled
                                {
                                    numOfTeams = 0;
                                }

                                // Read teams places (in case there are teams)
                                teamsIDs = new byte[numOfTeams];
                                teamsPlaces = new byte[numOfTeams];
                                for (int i = 0, loopTo3 = numOfTeams - 1; i <= loopTo3; i++)
                                {
                                    teamsIDs[i] = recv[currIndex];
                                    teamsPlaces[i] = recv[currIndex + 1];
                                    currIndex += 2;
                                }

                                // Read players places
                                byte numOfPlayers = recv[currIndex];
                                var playersIDs = new byte[numOfPlayers];
                                var playersPlaces = new byte[numOfPlayers];
                                for (int i = 0, loopTo4 = numOfPlayers - 1; i <= loopTo4; i++)
                                {
                                    playersIDs[i] = recv[currIndex];
                                    playersPlaces[i] = recv[currIndex + 1];
                                    currIndex += 2;
                                }

                                End_Of_Level_Event?.Invoke((byte)winnerID, winnerScore, playersIDs, playersPlaces, teamsIDs, teamsPlaces, UserData);
                                break;
                            }

                        case 0x3F: // Plus Info
                            {
                                if (packStartingIndex + 5 < recv.Length)
                                {
                                    _serverPlusVersion = BitConverter.ToInt32(recv, packStartingIndex + 2);
                                }

                                int boolsIndex = packStartingIndex + 9;
                                if (boolsIndex < recv.Length)
                                {
                                    _plusOnly = Conversions.ToBoolean(recv[boolsIndex] & 0x1);
                                    {
                                        var withBlock = PlusGameSettings;
                                        withBlock.PlusOnly = _plusOnly;
                                        withBlock.FriendlyFire = Conversions.ToBoolean(recv[boolsIndex] & 0x2);
                                        withBlock.NoMovement = Conversions.ToBoolean(recv[boolsIndex] & 0x4);
                                        withBlock.NoBliking = Conversions.ToBoolean(recv[boolsIndex] & 0x8);
                                        withBlock.ReadyCommandEnabled = Conversions.ToBoolean(recv[boolsIndex] & 0x32);
                                        withBlock.FireBall = Conversions.ToBoolean(recv[boolsIndex] & 0x32);
                                    }
                                }

                                if (boolsIndex + 1 < recv.Length) // *New settings*
                                {
                                    PlusGameSettings.WallJumping = Conversions.ToBoolean(recv[boolsIndex + 1] & 0x1);
                                }

                                break;
                            }

                        case 0x40: // Console Message
                            {
                                byte consoleMessageType = recv[packStartingIndex + 2];
                                var bytear = new[] { (byte)0x0 };
                                string str = Encoding.ASCII.GetString(bytear);
                                Console_Message_Recveived_Event?.Invoke(Encoding.UTF7.GetString(recv).Substring(packStartingIndex + 3, (int)((long)packetRealLength - (long)addnmbr - 3L)).Replace(Microsoft.VisualBasic.Constants.vbNullChar, ""), consoleMessageType, UserData);
                                break;
                            }

                        case 0x41:
                            {
                                byte packetType = recv[packStartingIndex + 2];
                                if (packetType != 0) // Single Spectator
                                {
                                    byte spectatorsSocketID = recv[packStartingIndex + 4];
                                    byte value = 0x1;
                                    if (recv[packStartingIndex + 5] == 0xFE)
                                    {
                                        value = 0x0;
                                        // SendMessage("/forcespectate " & JJ2ClientsSockInfo(spectatorsSocketID).playerID(0) + 1 & " on")
                                    }
                                    // If spectatorsSocketID < JJ2ClientsSockInfo.Length Then
                                    if (spectatorsSocketID < JJ2ClientsSockInfo.Length) // this expression might ruin. makes a missing spectator
                                    {
                                        if (JJ2ClientsSockInfo[spectatorsSocketID] is object)
                                        {
                                            JJ2ClientsSockInfo[spectatorsSocketID].IsSpectating = Conversions.ToBoolean(value);
                                            // raise event
                                            bool spectatorModeState;
                                            if (value == 0)
                                            {
                                                spectatorModeState = false;
                                            }
                                            else
                                            {
                                                spectatorModeState = true;
                                            }

                                            Client_Spectate_Event?.Invoke(spectatorModeState, spectatorsSocketID, UserData);
                                            foreach (byte anId in JJ2ClientsSockInfo[spectatorsSocketID].PlayerID)
                                            {
                                                if (anId != 0xFF)
                                                {
                                                    Player_Spectate_Event?.Invoke(spectatorModeState, anId, spectatorsSocketID, UserData);
                                                }
                                            }
                                        }
                                    }
                                }


                                // End If
                                else // Many Spectators represented by bits (1=ON)
                                {
                                    byte numOfBitsToRead = recv[packStartingIndex + 3];
                                    if (8 * (recv.Length - (packStartingIndex + 4)) >= numOfBitsToRead) // Checks if these is no missing bits
                                    {
                                        byte numOfBytes = (byte)Math.Round(Math.Ceiling(numOfBitsToRead / 8d));
                                        var BytesToBeConv = new byte[numOfBytes];
                                        Array.Copy(recv, packStartingIndex + 4, BytesToBeConv, 0, numOfBytes);
                                        var SpectatingPlayers = new BitArray(BytesToBeConv);
                                        for (byte i = 0, loopTo5 = (byte)(numOfBitsToRead - 1); i <= loopTo5; i++)
                                        {
                                            if (ActiveClients[i] == true & JJ2ClientsSockInfo[i] is object)
                                            {
                                                JJ2ClientsSockInfo[i].IsSpectating = SpectatingPlayers[i];
                                                // raise event
                                                bool spectatorModeState;
                                                if (Conversions.ToInteger(SpectatingPlayers[i]) == 0)
                                                {
                                                    spectatorModeState = false;
                                                }
                                                else
                                                {
                                                    spectatorModeState = true;
                                                }

                                                Client_Spectate_Event?.Invoke(spectatorModeState, i, UserData);
                                            }
                                            else
                                            {
                                                // Console.WriteLine("ERROR2! INVALID SPECTATOR ID,,, COULD NOT STORE " & i)
                                            }
                                        }
                                    }
                                }

                                break;
                            }

                        case 0x43: // Remote admins
                            {
                                if (false) // old code
                                {
                                    var intInBytes = new[] { recv[packStartingIndex + 4], recv[packStartingIndex + 5], recv[packStartingIndex + 6], recv[packStartingIndex + 7] };
                                    byte loggedOnSocketID = GetAdminSocketID(intInBytes);
                                    if (loggedOnSocketID == 0xFE)
                                    {
                                        for (byte i = 1; i <= 31; i++)
                                        {
                                            if (JJ2ClientsSockInfo[i] is object)
                                            {
                                                JJ2ClientsSockInfo[i].IsAdmin = false;
                                            }
                                        }

                                        JJ2ClientsSockInfo[0].IsAdmin = Conversions.ToBoolean(1);
                                    }
                                    // Richtextbox1AppendText("*Remote admin has been disabled" & vbNewLine)
                                    else if (loggedOnSocketID == 0xFF)
                                    {
                                    }
                                    else if (JJ2ClientsSockInfo[loggedOnSocketID] is object)
                                    {
                                        JJ2ClientsSockInfo[loggedOnSocketID].IsAdmin = true;
                                    }
                                }

                                if (true) // new code
                                {
                                    byte totalNumOfBits = recv[packStartingIndex + 3];
                                    for (int i = 0, loopTo6 = (int)Math.Round(Math.Ceiling(totalNumOfBits / 32d)) - 1; i <= loopTo6; i++)  // (read byte by byte)
                                    {
                                        byte currentByte = recv[packStartingIndex + 4 + i];
                                        for (int i2 = 0; i2 <= 8 - 1; i2++) // (byte = 8bits)
                                        {
                                            int clientIndex = i * 8 + i2;
                                            if (JJ2ClientsSockInfo[clientIndex] is object)
                                            {
                                                JJ2ClientsSockInfo[clientIndex].IsAdmin = Conversions.ToBoolean(currentByte & (long)Math.Round(Math.Pow(2d, i2)));
                                            }
                                        }
                                    }
                                }

                                Remote_Admins_Update_Event?.Invoke(UserData);
                                break;
                            }

                        case 0x44: // Clients State
                            {
                                // 13 44 00 20 00 0F 00 00 00 01 00 00 00 00 02 0F 00 00 00   <L=19
                                // LL 44 00 20 array{id,int32}

                                // There is an unknown byte comes before each collection of bits,
                                // mby it tells you what the bits are for,
                                // 0=activeArray 1=downloadingArray 2=hasPlusArray
                                // confirmed
                                /* TODO ERROR: Skipped IfDirectiveTrivia
                                #If DEBUG Then
                                */
                                if (recv[packStartingIndex + 2] != 0)
                                {
                                    Console.WriteLine("recved packet 0x44 but the third byte is " + recv[packStartingIndex + 2]);
                                }
                                /* TODO ERROR: Skipped EndIfDirectiveTrivia
                                #End If
                                */
                                int startIndex = packStartingIndex + 4;
                                while (startIndex + 4 < packStartingIndex + packetContentLength + 1L)
                                {
                                    byte dataType = recv[startIndex]; // 0=activeArray 1=downloadingArray 2=hasPlusArray
                                    var dataBytes = new[] { recv[startIndex + 1], recv[startIndex + 2], recv[startIndex + 3], recv[startIndex + 4] };
                                    var actualData = new BitArray(dataBytes);
                                    switch (dataType)
                                    {
                                        case 0: // active clients
                                            {
                                                for (int i = 0; i <= 32 - 1; i++)
                                                {
                                                    if (JJ2ClientsSockInfo[i] is object)
                                                        JJ2ClientsSockInfo[i].Active = actualData[i];
                                                }

                                                break;
                                            }

                                        case 1: // downloading clients
                                            {
                                                for (int i = 0; i <= 32 - 1; i++)
                                                {
                                                    if (JJ2ClientsSockInfo[i] is object)
                                                        JJ2ClientsSockInfo[i].IsDownloading = actualData[i];
                                                }

                                                break;
                                            }

                                        case 2: // client vanilla or plus
                                            {
                                                for (int i = 0; i <= 32 - 1; i++)
                                                {
                                                    if (JJ2ClientsSockInfo[i] is object)
                                                        JJ2ClientsSockInfo[i].Plus = actualData[i];
                                                }

                                                break;
                                            }
                                    }

                                    startIndex += 5;
                                }

                                if (false)
                                {
                                    var clientActiveBytes = new[] { recv[packStartingIndex + 5], recv[packStartingIndex + 6], recv[packStartingIndex + 7], recv[packStartingIndex + 8] };
                                    var isClientActive = new BitArray(clientActiveBytes);
                                    var isClientDownloadingBytes = new[] { recv[packStartingIndex + 10], recv[packStartingIndex + 11], recv[packStartingIndex + 12], recv[packStartingIndex + 13] };
                                    var isClientDownloading = new BitArray(isClientDownloadingBytes);
                                    var clientHasJJ2PlusBytes = new[] { recv[packStartingIndex + 15], recv[packStartingIndex + 16], recv[packStartingIndex + 17], recv[packStartingIndex + 18] };
                                    var clientHasJJ2Plus = new BitArray(clientHasJJ2PlusBytes);
                                    for (int i = 0; i <= 32 - 1; i++)
                                    {
                                        if (JJ2ClientsSockInfo[i] is object)
                                        {
                                            JJ2ClientsSockInfo[i].Plus = clientHasJJ2Plus[i];
                                            JJ2ClientsSockInfo[i].IsDownloading = isClientDownloading[i];
                                            JJ2ClientsSockInfo[i].Active = isClientActive[i];
                                        }
                                    }
                                }

                                Clients_State_Update_Event?.Invoke(UserData);
                                break;
                            }

                        case 0x45:
                            {
                                bool newOrEndedGame = false;
                                timerInfoUpdateDate = DateTime.Now;
                                bool gameWasStarted = _gameStarted;
                                _gameStarted = Conversions.ToBoolean(recv[packStartingIndex + 2] & 0x1);
                                _gameState = (JJ2.JJ2Plus_Game_State)(recv[packStartingIndex + 2] >> 1);
                                TimeRemaining = BitConverter.ToInt32(recv, packStartingIndex + 3);
                                TimeLimit = BitConverter.ToInt32(recv, packStartingIndex + 7);
                                if (_gameState != JJ2.JJ2Plus_Game_State.NORMAL)
                                {
                                    if (_gameStarted == false)
                                    {
                                        if (TimeRemaining == TimeLimit)
                                        {
                                            if (_gameInProgress)
                                            {
                                                // Current game Ended
                                                if (gameWasStarted == false)
                                                {
                                                    // ACCURATE Current game Ended
                                                    // (gg)
                                                }

                                                _gameInProgress = false;
                                                newOrEndedGame = true;
                                            }
                                            else
                                            {
                                                // first time??
                                                newOrEndedGame = false;
                                            }
                                        }
                                        else
                                        {
                                            // Game paused
                                            _gameInProgress = true;
                                            newOrEndedGame = false;
                                        }
                                    }
                                    else if (_gameInProgress)
                                    {
                                        // Game resumed
                                        newOrEndedGame = false;
                                    }
                                    else
                                    {
                                        // New game started
                                        newOrEndedGame = true;
                                        _gameInProgress = true;
                                        if (_isFirstGameState)
                                        {
                                        }
                                    }
                                }
                                else // if timelimit is disabled
                                {
                                    _gameInProgress = Conversions.ToBoolean(_gameState);
                                    newOrEndedGame = false;
                                }

                                Game_State_Changed_Event?.Invoke(_gameStarted, gameWasStarted, _gameState, TimeRemaining, TimeLimit, newOrEndedGame, _isFirstGameState, UserData);
                                _isFirstGameState = false;
                                break;
                            }

                        case 0x47:
                            {
                                break;
                            }
                        // 06 47 00 00 00 00 

                        case 0x49: // Pings
                            {
                                byte numOfPingedPlayers = (byte)Math.Round((packetRealLength - addnmbr - 2L) / 3d);
                                byte workingOnPlayer = 0x1;
                                int i = packStartingIndex + 2;
                                while (workingOnPlayer <= numOfPingedPlayers)
                                {
                                    byte playerID = recv[i];
                                    i += 1;
                                    var pingInBytes = new[] { (byte)0, (byte)0 };
                                    pingInBytes[0] = recv[i];
                                    i += 1;
                                    pingInBytes[1] = recv[i];
                                    i += 1;
                                    int Ping = BitConverter.ToInt16(pingInBytes, 0);
                                    if (Players[playerID] is object)
                                    {
                                        Players[playerID].latency = (short)Ping;
                                    }

                                    workingOnPlayer = (byte)(workingOnPlayer + 1);
                                }

                                Latency_Update_Event?.Invoke(UserData);
                                break;
                            }

                        case 0x4A: // Game Settings
                            {
                                // 09 4A 05 0B 14 00 00 00 0B 
                                // 09 4A gameMode customGameMode maxScore[4] enabledTeamsBitwise
                                _gameType = recv[packStartingIndex + 2];
                                _customGameMode = recv[packStartingIndex + 3];
                                _maxScore = BitConverter.ToInt32(recv, packStartingIndex + 4);
                                byte teamsState = recv[packStartingIndex + 8];
                                for (int teamID = 0; teamID <= 4 - 1; teamID++)
                                {
                                    bool teamEnabled = Conversions.ToBoolean(teamsState >> teamID & 0x1);
                                    if (Teams[teamID].Enabled != teamEnabled) // If changed
                                    {
                                        Teams[teamID].Enabled = teamEnabled;
                                        Team_State_Change_Event?.Invoke((byte)teamID, teamEnabled, UserData);
                                    }
                                }

                                Game_Settings_Update_Event?.Invoke((JJ2.JJ2_Game_Type)_gameType, (JJ2.JJ2_Custom_Game_Type)_customGameMode, _maxScore, UserData);
                                Console.WriteLine(_gameType + " " + _customGameMode + " " + _maxScore);
                                break;
                            }

                        case 0x4B: // Team score info update
                            {
                                // subpacket: 00 4B unknown1 unknown2 numOfTeamScoreInfo array(numOfTeamScoreInfo){teamID, Score[4]}
                                byte numOfTeamScoreInfo = recv[packStartingIndex + 4];
                                var teamsUpdated = new List<byte>(); // for the event
                                int arrayStartIndex = packStartingIndex + 5;
                                for (int i = 0, loopTo7 = numOfTeamScoreInfo - 1; i <= loopTo7; i++)
                                {
                                    // array{teamID, Score[4]}
                                    byte teamID = recv[arrayStartIndex];
                                    int oldScore = Teams[teamID].Score;
                                    Teams[teamID].Score = BitConverter.ToInt32(recv, arrayStartIndex + 1);
                                    teamsUpdated.Add(teamID);
                                    Gameplay_Team_Score_Set_Event?.Invoke(teamID, oldScore, Teams[(int)teamID].Score, UserData);
                                    arrayStartIndex += 5;
                                    Console.WriteLine("[TCP] score team" + teamID + "=" + Teams[(int)teamID].Score);
                                }

                                Gameplay_Teams_Scores_Set_Event?.Invoke(teamsUpdated.ToArray(), UserData);
                                break;
                            }

                        case 0x4C: // Player stats list
                            {
                                // 00 4C array{playerID, Roasts[4], Deaths[4]} 
                                int numOfPlayers = (int)Math.Round((packetContentLength - 1L) / 9d); // (packetContentLength - 1) is data length without JJ2 TCP packet header, 9 is array length
                                int arrayStartIndex = packStartingIndex + 2;
                                for (int i = 1, loopTo8 = numOfPlayers; i <= loopTo8; i++)
                                {
                                    if (arrayStartIndex + 8 >= recv.Length) // if packet is not long enough/invalid
                                    {
                                        break;
                                    }

                                    byte playerID = recv[arrayStartIndex];
                                    Players[playerID].Roasts = BitConverter.ToInt32(recv, arrayStartIndex + 1);
                                    Players[playerID].Deaths = BitConverter.ToInt32(recv, arrayStartIndex + 5);
                                    arrayStartIndex += 9; // 9 is array length
                                }

                                Gameplay_Player_Stats_List_Update_Event?.Invoke(UserData);
                                break;
                            }

                        case 0x4D: // Player deaths update
                            {
                                byte playerID = recv[packStartingIndex + 2];
                                if (playerID < Players.Length && Players[playerID] is object) // Check if not null
                                {
                                    Players[playerID].Deaths = BitConverter.ToInt32(recv, packStartingIndex + 3);
                                    Gameplay_Player_Deaths_Update_Event?.Invoke(playerID, Players[(int)playerID].Deaths, UserData);
                                    Console.WriteLine("player [" + Players[(int)playerID].Name + "] D=" + Players[(int)playerID].Deaths);
                                }
                                else
                                {
                                    Gameplay_Player_Deaths_Update_Event?.Invoke(playerID, 0, UserData);
                                    Console.WriteLine("player[" + playerID + "] D=" + BitConverter.ToInt32(recv, packStartingIndex + 3));
                                }

                                break;
                            }

                        case 0x51:
                            {
                                /* TODO ERROR: Skipped IfDirectiveTrivia
                                #If DEBUG Then
                                */
                                Console.WriteLine("Received packet 0x51");
                                break;
                            }
                        /* TODO ERROR: Skipped EndIfDirectiveTrivia
                        #End If
                        */
                        case 0x52: // Idle server mode
                            {
                                _idleServerMode = Conversions.ToBoolean(recv[packStartingIndex + 2]);
                                if (JJ2ClientsSockInfo[0] is object)
                                    JJ2ClientsSockInfo[0].IsIdle = _idleServerMode;
                                Idle_Server_Mode_Update_Event?.Invoke(_idleServerMode, UserData);
                                break;
                            }

                        case 0x55: // Set max resolution
                            {
                                if (packStartingIndex + 5 < recv.Length)
                                {
                                    ushort w = BitConverter.ToUInt16(recv, packStartingIndex + 2);
                                    ushort h = BitConverter.ToUInt16(recv, packStartingIndex + 4);
                                    PlusGameSettings.MaxResolutionWidth = w;
                                    PlusGameSettings.MaxResolutionHeight = h;
                                    Max_Resolution_Set_Event?.Invoke(w, h, UserData);
                                }

                                break;
                            }

                        case 0x56: // Script packet
                            {
                                byte plusNetworkStreamSourceId = recv[packStartingIndex + 2]; // mut id (not sure)
                                var fullRecvPacket = new byte[(int)(packetRealLength - 1L + 1)];
                                int plusNetworkStreamLength = fullRecvPacket.Length - (3 - addnmbr);
                                var plusNetworkStreamData = new byte[plusNetworkStreamLength + 1];
                                Array.Copy(recv, Starting, fullRecvPacket, 0L, fullRecvPacket.Length);
                                Array.Copy(fullRecvPacket, 3 + addnmbr, plusNetworkStreamData, 0, plusNetworkStreamLength);
                                JJ2_Plus_Network_Stream_Data_Arrival?.Invoke(plusNetworkStreamData, plusNetworkStreamSourceId);
                                break;
                            }

                        case 0x58: // Whisper
                            {
                                if (recv[(int)(Starting + 2L)] < 0x64) // recv(Starting + 2) is player ID
                                {
                                    // 'Received whisper from player
                                    byte sourcePlayerID = recv[(int)(Starting + 2L)];
                                    string message = DefaultEncoding.GetString(recv, (int)(Starting + 3L), (int)(packetRealLength - 3L));
                                }
                                else
                                {
                                    // 'Your whisper to player arrived
                                    byte recvPlayerID = (byte)(recv[(int)(Starting + 2L)] - 0x64);
                                }

                                break;
                            }

                        case 0x5A: // mut list
                            {
                                scriptModules.Clear();
                                scriptsRequiredFiles.Clear();
                                if (recv.Length - packStartingIndex >= 11)
                                {
                                    byte numOfScripts = recv[packStartingIndex + 7];
                                    ushort numOfRequiredFiles = (ushort)BitConverter.ToInt16(recv, packStartingIndex + 8);
                                    int fileStartIndex = packStartingIndex + 11;
                                    string fileName;
                                    byte scriptModuleId = 0;
                                    int temp;
                                    for (int i = 0, loopTo9 = numOfScripts + numOfRequiredFiles - 1; i <= loopTo9; i++)
                                    {
                                        int fileCRC32 = BitConverter.ToInt32(recv, fileStartIndex + 1);
                                        temp = fileStartIndex + 5; // file name length byte index.
                                        if (recv[temp] + temp < recv.Length)
                                        {
                                            fileName = DefaultEncoding.GetString(recv, fileStartIndex + 6, recv[temp]).ToLower();
                                            if (recv[fileStartIndex] == 0) // script
                                            {
                                                if (fileName.EndsWith(".mut"))
                                                {
                                                    scriptModuleId = (byte)(scriptModuleId + 1);
                                                }

                                                scriptModules.Add(fileName, scriptModuleId);
                                            }
                                            else // requiredFile
                                            {
                                                scriptsRequiredFiles.Add(fileName);
                                            }
                                        }
                                        else
                                        {
                                            Console.WriteLine("missing files 0 to " + (numOfScripts + numOfRequiredFiles) + " - " + numOfScripts + " " + numOfRequiredFiles + " " + recv[packStartingIndex + 8]);
                                            break;
                                        }

                                        fileStartIndex += recv[temp] + 6;
                                    }
                                }

                                break;
                            }
                    }



                    // ''''''''''''
                    // 'Ending the while loop:
                    if (Starting + packetRealLength >= recv.Length)
                    {
                        readingComplete = true;
                    }
                    else
                    {
                        Starting = Starting + packetRealLength;
                    }

                    packetLength = 0U;
                    packetRealLength = 0U;
                    addnmbr = 0;
                    packetNum += 1;

                    // End If ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                }
            }
            // '''''''Catch ex As Exception
            // '''''''RaiseEvent Error_Event(False, 2, ex.Message, UserData)
            // '''''''Finally
            // '''''''End Try
        }

        private void Winsock1_DataArrival_Read(byte[] recv)
        {
            try
            {
                if (recv.Length >= 2)
                {
                    int packetLength = 0;
                    int packetRealLength = 0;
                    int Starting = 0;
                    int addnmbr = 0;
                    byte packetID = 0x0;
                    int packetNum = 0;
                    bool readingComplete = false;
                    int packStartingIndex = 0;
                    while (readingComplete == false)
                    {
                        // If recv.Length > Starting + addnmbr + 1 Then '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                        if (!(Starting + 0 < recv.Length)) // new thing prevents OutOfRange reading
                        {
                            break;
                        }

                        if (recv[Starting + 0] == 0)
                        {
                            if (Starting + 1 == recv.Length)
                            {
                                break;
                            }
                            else if (Starting + 1 < recv.Length)
                            {
                                if (recv[Starting + 1] == 0)
                                {
                                    break;
                                }
                            }

                            var packetLengthInBytes = new[] { recv[Starting + 1], recv[Starting + 2] };
                            packetLength = BitConverter.ToInt16(packetLengthInBytes, 0);
                            packetRealLength = BitConverter.ToInt16(packetLengthInBytes, 0) + 3;
                            addnmbr = 2;
                        }

                        if (recv[Starting + 0] != 0)
                        {
                            if (recv[Starting] > recv.Length - Starting)
                            {
                                break;
                            }

                            packetLength = recv[Starting + 0];
                            packetRealLength = recv[Starting + 0];
                            addnmbr = 0;
                        }

                        packStartingIndex = Starting + addnmbr;
                        packetID = recv[packStartingIndex + 1];
                        // ''''''''''''

                        switch (packetID)
                        {
                            case 0x1B: // Chat message
                                {
                                    byte socketIDandTeam = recv[Starting + addnmbr + 2];
                                    byte senderSocketID = (byte)(socketIDandTeam & 15);
                                    byte team = (byte)Math.Round(Math.Floor(socketIDandTeam / 16d));
                                    string message = Encoding.Default.GetString(recv, Starting + addnmbr + 4, packetLength - 4);
                                    byte senderPlayerID = JJ2ClientsSockInfo[senderSocketID].PlayerID[0];
                                    if (senderPlayerID != 0xFF)
                                    {
                                        Message_Received_Event?.Invoke(message, Players[(int)senderPlayerID].Name, team, senderPlayerID, senderSocketID, UserData);
                                    }

                                    break;
                                }

                            case 0x12:
                                {
                                    byte totNumOfPlayers = recv[Starting + addnmbr + 2];
                                    var numOfPlayersDoneWithInSock = new byte[65];
                                    if (totNumOfPlayers != 0)
                                    {
                                        short playerArrStartingIndex = (short)(Starting + addnmbr + 3);
                                        for (byte workingOnPlayer = 0, loopTo = (byte)(totNumOfPlayers - 1); workingOnPlayer <= loopTo; workingOnPlayer++)
                                        {
                                            byte playerSocketIndex = recv[playerArrStartingIndex];
                                            byte playerNumber = recv[playerArrStartingIndex + 1];
                                            byte charTeam = recv[playerArrStartingIndex + 2];
                                            if (playerSocketIndex < JJ2ClientsSockInfo.Length & playerNumber < Players.Length)
                                            {
                                                // init socket info
                                                if (JJ2ClientsSockInfo[playerSocketIndex] is null)
                                                {
                                                    JJ2ClientsSockInfo[playerSocketIndex] = new JJ2.JJ2SocketInfo();
                                                }
                                                else
                                                {
                                                    // JJ2ClientsSockInfo(playerSocketIndex).reset()
                                                }

                                                JJ2ClientsSockInfo[playerSocketIndex].PlayerID[numOfPlayersDoneWithInSock[playerSocketIndex]] = playerNumber;
                                                numOfPlayersDoneWithInSock[playerSocketIndex] = (byte)(numOfPlayersDoneWithInSock[playerSocketIndex] + 1);
                                                ActiveClients[playerSocketIndex] = true;

                                                // init player
                                                if (Players[playerNumber] is null)
                                                {
                                                    Players[playerNumber] = new JJ2.JJ2Player(playerSocketIndex, (byte)(charTeam % 0x10), (byte)Math.Round(Math.Floor(charTeam / 16d)), JJ2ClientsSockInfo[playerSocketIndex]);
                                                }
                                                else
                                                {
                                                    // Players(playerNumber).reset()
                                                    Players[playerNumber].Update(playerSocketIndex, (byte)(charTeam % 0x10), (byte)Math.Round(Math.Floor(charTeam / 16d)), JJ2ClientsSockInfo[playerSocketIndex]);
                                                }

                                                Array.Copy(recv, (int)playerArrStartingIndex + 3, Players[(int)playerNumber].Color, 0, 4);
                                                byte playerNameLength = 0;
                                                byte whileHelper = (byte)(playerArrStartingIndex + 7);
                                                while (recv[whileHelper] != 0x0)
                                                {
                                                    playerNameLength = (byte)(playerNameLength + 1);
                                                    whileHelper = (byte)(whileHelper + 1);
                                                    if (recv.Length == whileHelper)
                                                    {
                                                        break;
                                                    }
                                                }

                                                Players[playerNumber].Name = Encoding.UTF7.GetString(recv, playerArrStartingIndex + 7, playerNameLength);

                                                // assign NumOfPlayers
                                                byte tempNumOfPlayersFromClient = 0;
                                                foreach (byte b in JJ2ClientsSockInfo[playerSocketIndex].PlayerID)
                                                {
                                                    if (b != 0xFF)
                                                    {
                                                        tempNumOfPlayersFromClient = (byte)(tempNumOfPlayersFromClient + 1);
                                                    }
                                                }

                                                JJ2ClientsSockInfo[playerSocketIndex].NumOfPlayers = tempNumOfPlayersFromClient;
                                                playerArrStartingIndex = (short)(playerArrStartingIndex + (8 + playerNameLength));
                                                // MsgBox(Players(playerNumber).Name & " in server")
                                            }
                                        }
                                    }

                                    break;
                                }

                            case 0x11:
                                {
                                    byte joinedClientSocketIndex = recv[Starting + addnmbr + 2];
                                    byte numIfJoinedPlayers = recv[Starting + addnmbr + 3];
                                    ushort playerArrStartingIndex = (ushort)(Starting + addnmbr + 4);
                                    if (joinedClientSocketIndex < _connectionlimit)
                                    {
                                        // init socket info
                                        if (JJ2ClientsSockInfo[joinedClientSocketIndex] is null)
                                        {
                                            JJ2ClientsSockInfo[joinedClientSocketIndex] = new JJ2.JJ2SocketInfo();
                                        }
                                        else
                                        {
                                            JJ2ClientsSockInfo[joinedClientSocketIndex].reset();
                                        }

                                        for (byte workingOnPlayer = 0, loopTo1 = (byte)(numIfJoinedPlayers - 1); workingOnPlayer <= loopTo1; workingOnPlayer++)
                                        {
                                            byte playerNumber = recv[playerArrStartingIndex];
                                            byte charTeam = recv[playerArrStartingIndex + 1];
                                            if (joinedClientSocketIndex < JJ2ClientsSockInfo.Length & playerNumber < Players.Length)
                                            {
                                                JJ2ClientsSockInfo[joinedClientSocketIndex].PlayerID[workingOnPlayer] = playerNumber;
                                                byte fdgfdgdfgdfgfdg = JJ2ClientsSockInfo[joinedClientSocketIndex].PlayerID[workingOnPlayer];

                                                // init player
                                                if (Players[playerNumber] is null)
                                                {
                                                    Players[playerNumber] = new JJ2.JJ2Player(joinedClientSocketIndex, (byte)(charTeam % 0x10), (byte)Math.Round(Math.Floor(charTeam / 16d)), JJ2ClientsSockInfo[joinedClientSocketIndex]);
                                                }
                                                else
                                                {
                                                    Players[playerNumber].reset();
                                                    Players[playerNumber] = new JJ2.JJ2Player(joinedClientSocketIndex, (byte)(charTeam % 0x10), (byte)Math.Round(Math.Floor(charTeam / 16d)), JJ2ClientsSockInfo[joinedClientSocketIndex]);
                                                }

                                                Array.Copy(recv, (int)playerArrStartingIndex + 2, Players[(int)playerNumber].Color, 0, 4);
                                                byte playerNameLength = 0;
                                                byte whileHelper = (byte)(playerArrStartingIndex + 6);
                                                while (recv[whileHelper] != 0x0)
                                                {
                                                    playerNameLength = (byte)(playerNameLength + 1);
                                                    whileHelper = (byte)(whileHelper + 1);
                                                    if (recv.Length == whileHelper)
                                                    {
                                                        break;
                                                    }
                                                }

                                                Players[playerNumber].Name = Encoding.UTF7.GetString(recv, playerArrStartingIndex + 6, playerNameLength);
                                                JJ2ClientsSockInfo[joinedClientSocketIndex].NumOfPlayers = (byte)(JJ2ClientsSockInfo[joinedClientSocketIndex].NumOfPlayers + 1);
                                                playerArrStartingIndex = (ushort)(playerArrStartingIndex + (7 + playerNameLength));
                                                Player_Joined_Event?.Invoke(Players[(int)playerNumber].Name, playerNumber, joinedClientSocketIndex, (byte)Players[(int)playerNumber].Character, Players[(int)playerNumber].Team, UserData);
                                            }
                                        }

                                        ActiveClients[joinedClientSocketIndex] = true;
                                        Client_Connected_Event?.Invoke(joinedClientSocketIndex, JJ2ClientsSockInfo[(int)joinedClientSocketIndex].NumOfPlayers, UserData);
                                    }

                                    break;
                                }

                            case 0xD:
                                {
                                    byte disconnectMsg = recv[Starting + addnmbr + 2];
                                    byte leftClientSocketIndex = recv[Starting + addnmbr + 3];
                                    if (leftClientSocketIndex != socketID)
                                    {
                                        if (leftClientSocketIndex < JJ2ClientsSockInfo.Length)
                                        {
                                            ActiveClients[leftClientSocketIndex] = false;
                                            if (JJ2ClientsSockInfo[leftClientSocketIndex] is object)
                                            {
                                                if (Conversions.ToBoolean(JJ2ClientsSockInfo[leftClientSocketIndex].NumOfPlayers))
                                                {
                                                    Client_Disconnected_Event?.Invoke(leftClientSocketIndex, (JJ2.JJ2_Disconnect_Message)disconnectMsg, JJ2ClientsSockInfo[(int)leftClientSocketIndex].NumOfPlayers, UserData);
                                                    for (byte wokingOnPlayer = 0, loopTo2 = (byte)((int)JJ2ClientsSockInfo[leftClientSocketIndex].NumOfPlayers - 1); wokingOnPlayer <= loopTo2; wokingOnPlayer++)
                                                    {
                                                        if ((int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer] != 0xFF & (int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer] < Players.Length + 1)
                                                        {
                                                            string playerName = Players[(int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer]].Name;
                                                            byte playerID = JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer];
                                                            if (Players[(int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer]] is object)
                                                            {
                                                                Players[(int)JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer]].reset();
                                                            }

                                                            JJ2ClientsSockInfo[leftClientSocketIndex].PlayerID[wokingOnPlayer] = 0xFF;
                                                            Player_Left_Event?.Invoke(playerName, (JJ2.JJ2_Disconnect_Message)disconnectMsg, playerID, leftClientSocketIndex, UserData);
                                                        }
                                                    }

                                                    JJ2ClientsSockInfo[leftClientSocketIndex].reset();
                                                }
                                            }
                                        }
                                    }
                                    else
                                    {
                                        // You
                                        WinsockClose(disconnectMsg);
                                    }

                                    break;
                                }

                            case 0x10:
                                {
                                    socketID = recv[Starting + addnmbr + 2];
                                    byte levelNameLength = recv[Starting + addnmbr + 4];
                                    socketID = recv[Starting + addnmbr + 2];
                                    _ID = recv[Starting + addnmbr + 3];
                                    _nextLevelName = Encoding.ASCII.GetString(recv).Substring(packStartingIndex + 5, levelNameLength);
                                    var joiningData2Part1 = new[] { (byte)0, (byte)0xE, (byte)0x1, (byte)0x1, (byte)0x1, (byte)0x18, (byte)0x20, (byte)0x28, (byte)0x11 }; // {&H19, &HE, &H1, &H1, &H0, &H1, &H10, &H18, &H20, &H28, &H11, &H1, &HA, &HD, &H0, &H0, &H52, &H65, &H63, &H6F, &H72, &H64, &H65, &H72, &H0}
                                    var joiningData2Part2 = DefaultNameEncoding.GetBytes(_name);
                                    var joiningData2 = new byte[joiningData2Part1.Length + joiningData2Part2.Length + 1];
                                    Array.Copy(joiningData2Part1, joiningData2, joiningData2Part1.Length);
                                    Array.Copy(joiningData2Part2, 0, joiningData2, joiningData2Part1.Length, joiningData2Part2.Length);
                                    joiningData2[0] = (byte)joiningData2.Length;
                                    if (!_packet0x0EWasSent)
                                    {
                                        _packet0x0EWasSent = true;
                                        Winsock1SendData(joiningData2);
                                    }

                                    break;
                                }

                            case 0x16:
                                {
                                    if (recv.Length - Starting - addnmbr > 11)
                                    {
                                        if (recv.Length - Starting - addnmbr >= 11 + recv[10 + Starting]) // recv(10)=lvlNamwLeng
                                        {
                                            _nextLevelName = Encoding.ASCII.GetString(recv, Starting + addnmbr + 11, recv[10 + Starting]);
                                            ChangeLevel(_nextLevelName, 0);
                                        }
                                    }

                                    break;
                                }

                            case 0x17:
                                {
                                    _cycling = true;
                                    int winnerID = recv[packStartingIndex + 2];
                                    int winnerScore = recv[packStartingIndex + 7];
                                    var teamsIDs = Array.Empty<byte>(); // for JJ2+ only
                                    var teamsPlaces = Array.Empty<byte>(); // for JJ2+ only
                                    var playersIDs = Array.Empty<byte>(); // for JJ2+ only
                                    var playersPlaces = Array.Empty<byte>(); // for JJ2+ only
                                    if (_gameType == (int)JJ2.JJ2_Game_Type.CAPTURE)
                                    {
                                        teamsIDs = new[] { (byte)winnerID };
                                        teamsPlaces = new[] { (byte)1 };
                                    }
                                    else
                                    {
                                        playersIDs = new[] { (byte)winnerID };
                                        playersPlaces = new[] { (byte)1 };
                                    }

                                    End_Of_Level_Event?.Invoke((byte)winnerID, winnerScore, playersIDs, playersPlaces, teamsIDs, teamsPlaces, UserData);
                                    break;
                                }

                            case 0x13:
                                {
                                    ChangeLevel(_nextLevelName, 0);
                                    break;
                                }

                            case 151:
                                {
                                    break;
                                }
                        }



                        // ''''''''''''
                        // 'Ending the while loop:
                        if (Starting + packetRealLength >= recv.Length)
                        {
                            readingComplete = true;
                        }
                        else
                        {
                            Starting = Starting + packetRealLength;
                        }

                        packetLength = 0;
                        packetRealLength = 0;
                        addnmbr = 0;
                        packetNum += 1;

                        // End If ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                    }
                }
            }
            catch (Exception ex)
            {
                Error_Event?.Invoke(false, 1, ex.Message, UserData);
            }
        }

        private void Winsock2GoReceive()
        {
            try
            {
                EndPoint argremoteEP = _remoteEP;
                Winsock2.Client.BeginReceiveFrom(BufferUDP, 0, BufferUDP.Length, SocketFlags.None, ref argremoteEP, new AsyncCallback(Winsock2_DataArrival), _remoteEP);
            }
            catch (SocketException sockEx)
            {
                WinsockClose(7);
            }
            catch (NullReferenceException nullEx)
            {
            }
            catch (ObjectDisposedException obDisEx)
            {
            }
        }

        private byte[] BufferUDP = new byte[512];

        private void Winsock2_DataArrival(IAsyncResult ar)
        {
            try
            {
                EndPoint argendPoint = (EndPoint)ar.AsyncState;
                int dataLength = Winsock2.Client.EndReceiveFrom(ar, ref argendPoint);
                TotalBytesRecv = (ulong)Math.Round(TotalBytesRecv + (decimal)dataLength);
                ConnectionTimeOut = 30;
                var recv = new byte[dataLength];
                Array.Copy(BufferUDP, recv, dataLength);
                if (_plusServer == false)
                {
                    Winsock2_DataArrival_Read(recv);
                }
                else
                {
                    Winsock2_DataArrival_Read_Plus(recv);
                }

                Winsock2GoReceive();
            }
            catch (SocketException sockEx)
            {
                WinsockClose(7);
            }
            // MsgBox("Winsock2_DataArrival err")
            catch (NullReferenceException nullEx)
            {
            }
            catch (ObjectDisposedException obDisEx)
            {
            }
        }

        private void Winsock2_DataArrival_Read(byte[] recv)
        {
            try
            {
                if (_connected && recv.Length > 2)
                {
                    byte packetID = recv[2];
                    switch (packetID)
                    {
                        case 0x7:
                            {
                                int subpacketStartIndex = 4;
                                int subpacketLength;
                                byte subpacketID;
                                while (subpacketStartIndex < recv.Length)
                                {
                                    subpacketID = recv[subpacketStartIndex];
                                    subpacketLength = Conversions.ToInteger(GetGameplaySubpacketLength(subpacketID));
                                    if (subpacketLength <= 0)
                                    {
                                        break;
                                    }

                                    switch (subpacketID)
                                    {
                                        case 0x7: // Bullet fired
                                            {
                                                break;
                                            }

                                        case 0xA:
                                            {
                                                break;
                                            }

                                        case 0xC: // Hurt
                                            {
                                                break;
                                            }

                                        case 0xE: // Kill
                                            {
                                                byte victimID = recv[subpacketStartIndex + 1];
                                                int victimKills = recv[subpacketStartIndex + 2];
                                                byte killerID = recv[subpacketStartIndex + 3];
                                                int killerKills = recv[subpacketStartIndex + 4];
                                                if (victimID < Players.Length)
                                                {
                                                    Players[victimID].Roasts = victimKills;
                                                }

                                                if (killerID < Players.Length)
                                                {
                                                    Players[killerID].Roasts = killerKills;
                                                }

                                                Gameplay_Player_Roast_Event?.Invoke(victimID, victimKills, killerID, killerKills, UserData);
                                                break;
                                            }

                                        case 0xF: // Hit by special move
                                            {
                                                byte victimID = recv[subpacketStartIndex + 1] & 32 - 1;
                                                byte victimHealth = recv[subpacketStartIndex + 1] & 0xE0;
                                                byte attackerID = recv[subpacketStartIndex + 2] & 32 - 1;
                                                Gameplay_Player_Hit_Event?.Invoke(victimID, victimHealth, attackerID, UserData);
                                                break;
                                            }

                                        case 0x1E: // CTF info
                                            {
                                                byte redCarier;
                                                byte blueCarier;
                                                break;
                                            }
                                    }

                                    subpacketStartIndex += 1 + subpacketLength;
                                }

                                break;
                            }

                        case 0x9:
                            {
                                if (recv.Length > 8)
                                {
                                    var Packet9SendBack = new[] { (byte)0, (byte)0, (byte)0x9, UDPCount, (byte)0, recv[5], recv[6], recv[7], recv[8] };
                                    UDPchecksum(Packet9SendBack);
                                    if (ExtraLatency > 0)
                                    {
                                        System.Threading.Thread.Sleep(ExtraLatency);
                                    }

                                    Winsock2SendData(Packet9SendBack);
                                }

                                break;
                            }
                    }
                }
            }
            catch (Exception ex)
            {
                Error_Event?.Invoke(false, 3, ex.Message, UserData);
            }
            finally
            {
            }
        }

        private void Winsock2_DataArrival_Read_Plus(byte[] recv)
        {
            try
            {
                if (_connected && recv.Length > 2)
                {
                    byte packetID = recv[2];
                    switch (packetID)
                    {
                        case 0x7:
                            {
                                int subpacketStartIndex = 4;
                                int subpacketLength;
                                byte subpacketID;
                                while (subpacketStartIndex < recv.Length)
                                {
                                    subpacketID = recv[subpacketStartIndex];
                                    subpacketLength = Conversions.ToInteger(GetPlusGameplaySubpacketLength(subpacketID));
                                    if (subpacketLength <= 0)
                                    {
                                        /* TODO ERROR: Skipped IfDirectiveTrivia
                                        #If DEBUG Then
                                        */
                                        Console.WriteLine("Length of subpacket [" + subpacketID + "] is unknown");
                                        /* TODO ERROR: Skipped EndIfDirectiveTrivia
                                        #End If
                                        */
                                        break;
                                    }

                                    switch (subpacketID)
                                    {
                                        case 0x3:
                                            {
                                                subpacketLength = 3; // for plus?
                                                break;
                                            }

                                        case 0x7: // Bullet fired
                                            {
                                                break;
                                            }

                                        case 0xA:
                                            {
                                                break;
                                            }
                                        // subpacketLength = 3 'for plus?
                                        case 0xC: // Hurt
                                            {
                                                break;
                                            }

                                        case 0xE: // Kill
                                            {
                                                byte victimID = recv[subpacketStartIndex + 1];
                                                int victimKills = BitConverter.ToInt32(recv, subpacketStartIndex + 2);
                                                byte killerID = recv[subpacketStartIndex + 6];
                                                int killerKills = BitConverter.ToInt32(recv, subpacketStartIndex + 7);
                                                if (victimID < Players.Length)
                                                {
                                                    Players[victimID].Roasts = victimKills;
                                                }

                                                if (killerID < Players.Length)
                                                {
                                                    Players[killerID].Roasts = killerKills;
                                                }

                                                Gameplay_Player_Roast_Event?.Invoke(victimID, victimKills, killerID, killerKills, UserData);
                                                break;
                                            }

                                        case 0xF: // Hit by special move
                                            {
                                                byte victimID = recv[subpacketStartIndex + 1] & 32 - 1;
                                                byte victimHealth = (recv[subpacketStartIndex + 1] & 0xE0) >> 5;
                                                byte attackerID = recv[subpacketStartIndex + 2] & 32 - 1;
                                                Gameplay_Player_Hit_Event?.Invoke(victimID, victimHealth, attackerID, UserData);
                                                break;
                                            }

                                        case 0x10: // Bullet(Plus)??
                                            {
                                                subpacketLength = 20; // for plus?
                                                byte gun = (byte)(recv[subpacketStartIndex + 7] & 0xF);
                                                bool isGunPU = Conversions.ToBoolean(recv[subpacketStartIndex + 7] & 0x80);
                                                bool vertical = Conversions.ToBoolean(recv[subpacketStartIndex + 7] & 0x40); // not sure????????
                                                short bulletX = BitConverter.ToInt16(recv, subpacketStartIndex + 8);
                                                short bulletY = BitConverter.ToInt16(recv, subpacketStartIndex + 10);

                                                // bullet velocity is not final, playVx will affect it
                                                sbyte bulletVx = (sbyte)(recv[subpacketStartIndex + 13] < 128 ? recv[subpacketStartIndex + 13] : recv[subpacketStartIndex + 13] - 256);
                                                sbyte bulletAngle = (sbyte)(recv[subpacketStartIndex + 14] < 128 ? recv[subpacketStartIndex + 14] : recv[subpacketStartIndex + 14] - 256); // a guess
                                                sbyte bulletVy = (sbyte)(recv[subpacketStartIndex + 15] < 128 ? recv[subpacketStartIndex + 15] : recv[subpacketStartIndex + 15] - 256);
                                                sbyte playVx = (sbyte)(recv[subpacketStartIndex + 16] < 128 ? recv[subpacketStartIndex + 16] : recv[subpacketStartIndex + 16] - 256); // i think
                                                sbyte direction = (sbyte)(recv[subpacketStartIndex + 17] < 128 ? recv[subpacketStartIndex + 17] : recv[subpacketStartIndex + 17] - 256);
                                                float actualAngle; // clockwise, starting form right (like some 2d game frameworks)
                                                byte lifetime = recv[subpacketStartIndex + 18];
                                                byte ammo = recv[subpacketStartIndex + 20];
                                                Gameplay_Plus_Bullet_Shoot_Event?.Invoke(gun, isGunPU, bulletX, bulletY, bulletVx, bulletVy, bulletAngle, playVx, lifetime, ammo, direction, vertical, 0xFF, false, UserData);
                                                break;
                                            }

                                        /* TODO ERROR: Skipped IfDirectiveTrivia
                                        #If DEBUG Then
                                        */                                     // Console.WriteLine("Aim val [" & recv(subpacketStartIndex + 14) & "]")
                                        /* TODO ERROR: Skipped EndIfDirectiveTrivia
                                        #End If
                                        */
                                        case 0x1E: // CTF info
                                            {
                                                // subpacket: 1E 00 02 numOfTeamInfo array(numOfTeamInfo){teamID, Score[4]} unknownByte numOfFlagInfo array(numOfFlagInfo){teamID, isFlagCaptured, carrierID}
                                                byte numOfTeamScoreInfo = recv[subpacketStartIndex + 3];
                                                var teamsUpdated = new byte[numOfTeamScoreInfo]; // for the event
                                                int arrayStartIndex = subpacketStartIndex + 4;
                                                for (int i = 0, loopTo = numOfTeamScoreInfo - 1; i <= loopTo; i++)
                                                {
                                                    // array{teamID, Score[4]}
                                                    byte teamID = recv[arrayStartIndex];
                                                    teamsUpdated[i] = teamID; // for event
                                                    int oldScore = Teams[teamID].Score;
                                                    Teams[teamID].Score = BitConverter.ToInt32(recv, arrayStartIndex + 1);
                                                    if (Teams[teamID].Score != oldScore)
                                                    {
                                                        Gameplay_Team_Scored_Event?.Invoke(teamID, oldScore, Teams[(int)teamID].Score, UserData);
                                                    }

                                                    Gameplay_Team_Score_Update_Event?.Invoke(teamID, oldScore, Teams[(int)teamID].Score, UserData);
                                                    arrayStartIndex += 5;
                                                    Console.WriteLine("score team" + teamID + "=" + Teams[(int)teamID].Score);
                                                }

                                                Gameplay_Teams_Scores_Update_Event?.Invoke(teamsUpdated, UserData);
                                                byte unknownByte = recv[arrayStartIndex];
                                                Console.WriteLine("unknwnByte=" + unknownByte);
                                                byte numOfFlagInfo = recv[arrayStartIndex + 1];
                                                var teamFlagsUpdated = new byte[numOfFlagInfo]; // for the event
                                                arrayStartIndex += 2;
                                                for (int i = 0, loopTo1 = numOfFlagInfo - 1; i <= loopTo1; i++)
                                                {
                                                    // array{teamID, isFlagCaptured, carrierID}
                                                    byte teamID = recv[arrayStartIndex];
                                                    teamFlagsUpdated[i] = teamID;
                                                    Teams[teamID].FlagIsCaptured = Conversions.ToBoolean(recv[arrayStartIndex + 1]);
                                                    if (Teams[teamID].FlagIsCaptured)
                                                    {
                                                        Teams[teamID].FlagCarriedByPlayerID = recv[arrayStartIndex + 2];
                                                        Console.WriteLine("flag" + teamID + " is carried by player " + Teams[(int)teamID].FlagCarriedByPlayerID);
                                                    }
                                                    else
                                                    {
                                                        Console.WriteLine("flag" + teamID + " is on the base");
                                                    }

                                                    Gameplay_CTF_Flag_Update_Event?.Invoke(teamID, Teams[(int)teamID].FlagIsCaptured, Teams[(int)teamID].FlagCarriedByPlayerID, UserData);
                                                    arrayStartIndex += 3;
                                                }

                                                Gameplay_CTF_Flags_Update_Event?.Invoke(teamFlagsUpdated, UserData);
                                                subpacketLength = arrayStartIndex - subpacketStartIndex - 1; // set subpacket length manually since it's not static
                                                break;
                                            }

                                        case 0x1F: // CTF info
                                            {
                                                subpacketLength = 3; // for plus?
                                                break;
                                            }

                                        case 0x22: // Bullet(Plus)??
                                            {
                                                break;
                                            }
                                            // subpacketLength = 16 'for plus?
                                            // Dim bulletX As Short = BitConverter.ToInt16(recv, +4)
                                            // Dim bulletY As Short = BitConverter.ToInt16(recv, +6)
                                            // Dim ammo As Byte = recv(subpacketStartIndex + 16)
                                    }

                                    subpacketStartIndex += 1 + subpacketLength;
                                }

                                break;
                            }

                        case 0x9:
                            {
                                if (recv.Length > 8)
                                {
                                    var Packet9SendBack = new[] { (byte)0, (byte)0, (byte)0x9, UDPCount, (byte)0, recv[5], recv[6], recv[7], recv[8] };
                                    UDPchecksum(Packet9SendBack);
                                    if (ExtraLatency > 0)
                                    {
                                        System.Threading.Thread.Sleep(ExtraLatency);
                                    }

                                    Winsock2SendData(Packet9SendBack);
                                }

                                break;
                            }
                    }
                }
            }
            catch (Exception ex)
            {
                Error_Event?.Invoke(false, 3, ex.Message, UserData);
            }
            finally
            {
            }
        }

        private static int[] GAMEPLAY_SUBPACKET_LENGTH = new[] { 0, 3, 3, 1, 2, 1, 1, 7, 12, 1, 2, 4, 4, 1, 4, 2, 5, 2, 2, 2, 2, 3, 4, 2, 3, 4, 3, 9, 10, 3, 4, 2, 7 };

        public static object GetGameplaySubpacketLength(byte subpacketID)
        {
            if (subpacketID < GAMEPLAY_SUBPACKET_LENGTH.Length)
            {
                return GAMEPLAY_SUBPACKET_LENGTH[subpacketID];
            }
            else
            {
                return 0;
            }
        }

        private static int[] GAMEPLAY_SUBPACKET_LENGTH_PLUS = new[] { 0, 3, 3, 1, 2, 1, 1, 7, 12, 1, 3, 4, 4, 1, 10, 2, 2, 2, 2, 2, 2, 3, 4, 2, 3, 4, 3, 9, 10, 3, 4, 2, 7 };

        public static object GetPlusGameplaySubpacketLength(byte subpacketID)
        {
            if (subpacketID < GAMEPLAY_SUBPACKET_LENGTH_PLUS.Length)
            {
                return GAMEPLAY_SUBPACKET_LENGTH_PLUS[subpacketID];
            }
            else
            {
                return 0;
            }
        }

        private byte antiSpamSecondsElapsed = 0;

        private void every1SecTimerTick(object sender, System.Timers.ElapsedEventArgs e)
        {
            // UDP
            if (udpTimerState)
            {
                if (ConnectionTimeOut != 0)
                {
                    ConnectionTimeOut = (byte)(ConnectionTimeOut - 1);
                }
                else
                {
                    udpTimerState = false;
                    WinsockClose(0x9);
                }
            }

            // Anti-Spam
            if (AntiSpam != false)
            {
                antiSpamSecondsElapsed = (byte)(antiSpamSecondsElapsed + 1);
                if (antiSpamSecondsElapsed >= _antiSpamClearSeconds)
                {
                    antiSpamSecondsElapsed = 0;
                    for (byte i = 0, loopTo = (byte)(JJ2ClientsSockInfo.Length - 1); i <= loopTo; i++)
                    {
                        if (ActiveClients[i])
                        {
                            if (JJ2ClientsSockInfo[i] is object)
                            {
                                JJ2ClientsSockInfo[i].AntiSpamCount = 0;
                            }
                        }
                    }
                }
            }
        }

        private void AnimTimerTick(object sender, System.Timers.ElapsedEventArgs e)
        {
            if (_connected)
            {
                posPacket[posPacket.Length - 3] = (byte)(posPacket[posPacket.Length - 3] + 1);
                if (posPacket[posPacket.Length - 3] == 5)
                {
                    posPacket[posPacket.Length - 3] = 0;
                }
                // 'Anti Idle
                if (posPacket[5] == 0xB5)
                {
                    posPacket[5] = 0xB6;
                }
                else
                {
                    posPacket[5] = 0xB5;
                }

                Winsock2SendData(posPacket);
            }
        }

        private void Winsock1SendData(byte[] data)
        {
            try
            {
                Winsock1.Send(data);
                TotalBytesSent = (ulong)Math.Round(TotalBytesSent + (decimal)data.Length);
            }
            catch (ObjectDisposedException objDisEx)
            {
                if (_connected)
                {
                    WinsockClose(7);
                }
            }
            catch (SocketException sockEx)
            {
                // MsgBox("Winsock1SendData")
                WinsockClose(7);
            }
        }

        private void Winsock2SendData(byte[] data)
        {
            try
            {
                UDPchecksum(data);
                Winsock2.Client.SendTo(data, _remoteEP);
                TotalBytesSent = (ulong)Math.Round(TotalBytesSent + (decimal)data.Length);
            }
            catch (SocketException exSock)
            {
                // Console.WriteLine("UDP Winsock Error.")
            }
        }

        private byte[] posPacket = new[] { (byte)0x3D, (byte)0xA7, (byte)0x1, (byte)0x0, (byte)0x1, (byte)0xB5, (byte)0x0, (byte)0x0, (byte)0x0, (byte)0x80, (byte)0x0, (byte)0x0, (byte)0x1E, (byte)0x0, (byte)0x0, (byte)0x0 };

        private void ChangeLevel(string levelName, int levelCRC32) // Loading screen equivalent
        {
            _currentLevelName = levelName;
            _levelCRC32 = levelCRC32;

            // clean
            for (byte i = 0, loopTo = (byte)(Players.Length - 1); i <= loopTo; i++)
            {
                if (Players[i] is object)
                    Players[i].ClearStats(_plusServer);
            }

            _isFirstScoreUpdate = true;
            _isFirstGameState = true;
            byte[] replyPacket;
            if (!PlusServer)
            {
                _gameInProgress = true;
                replyPacket = new[] { (byte)2, (byte)0x1A };
                udpTimerState = true;
            }
            else
            {
                _gameInProgress = false;
                _scriptsEnabled = _plusOnly;
                var joiningDataPlus4 = new[] { (byte)0x6, (byte)0x1A, PlusCheck[0], PlusCheck[1], PlusCheck[2], PlusCheck[3] };
                replyPacket = joiningDataPlus4;
            }

            _cycling = false;
            Winsock1SendData(replyPacket);
            if (_ID != 0xFF)
            {
                if (Players[_ID] is object)
                {
                    Level_Initialized_Event?.Invoke(_currentLevelName, Players[(int)_ID].Name, _ID, socketID, UserData);
                }
                else
                {
                    Error_Event?.Invoke(false, 1000, "Your player info (index=" + _ID + ") is NULL.", UserData);
                    Level_Initialized_Event?.Invoke(_currentLevelName, "", _ID, socketID, UserData);
                }
            }
        }

        public void SendJJ2PlusNetworkStream(byte[] streamData, byte scriptModuleId)
        {
            // max length is 252 temp
            if (streamData.Length <= 252)
            {
                var sendData = new byte[streamData.Length + 2 + 1];
                sendData[0] = (byte)sendData.Length;
                sendData[1] = 0x57;
                sendData[2] = scriptModuleId;
                Array.Copy(streamData, 0, sendData, 3, streamData.Length);
                Winsock1SendData(sendData);
            }
        }

        public byte GetScriptModuleId(string scriptName)
        {
            if (scriptModules.ContainsKey(scriptName))
            {
                return scriptModules[scriptName];
            }
            else
            {
                return 0xFF;
            }
        }

        private byte[] Combine2Arrays(byte[] a1, byte[] a2)
        {
            var list = new List<byte>();
            foreach (byte a in a1)
                list.Add(a);
            foreach (byte a in a2)
                list.Add(a);
            return list.ToArray();
        }

        private object Check_Client_connection(ref Socket sckt)
        {
            bool ClientStatus;
            if (sckt.Connected)
            {
                if (sckt.Available == 0 & sckt.Poll(1000, SelectMode.SelectRead) == true)
                {
                    ClientStatus = false;
                }
                else
                {
                    ClientStatus = true;
                }
            }
            else
            {
                ClientStatus = false;
            }

            return ClientStatus;
        }

        private static object UDPchecksum(byte[] buffer)
        {
            // 3.UDP checksum
            int x = 1;
            int y = 1;
            for (int i = 2, loopTo = buffer.Length - 1; i <= loopTo; i++)
            {
                x += buffer[i];
                y += x;
            }

            buffer[0] = (byte)(x % 251);
            buffer[1] = (byte)(y % 251);
            return default;
        }

        private byte GetAdminSocketID(byte[] @int)
        {
            byte result = 0xFF;
            int Int2;
            if (@int.Length == 2)
            {
                Int2 = BitConverter.ToInt16(@int, 0);
            }
            else if (@int.Length == 4)
            {
                Int2 = BitConverter.ToInt32(@int, 0);
            }
            else if (@int.Length == 8)
            {
                Int2 = (int)BitConverter.ToInt64(@int, 0);
            }
            else
            {
                goto giveResult;
            }

            switch (Int2)
            {
                case 0:
                    {
                        result = 0xFE;
                        break;
                    }

                case 2:
                    {
                        result = 1;
                        break;
                    }

                case 6:
                    {
                        result = 2;
                        break;
                    }

                case 10:
                    {
                        result = 3;
                        break;
                    }

                case 26:
                    {
                        result = 4;
                        break;
                    }

                case 58:
                    {
                        result = 5;
                        break;
                    }

                case 90:
                    {
                        result = 6;
                        break;
                    }

                case 218:
                    {
                        result = 7;
                        break;
                    }

                case 474:
                    {
                        result = 8;
                        break;
                    }

                case 986:
                    {
                        result = 9;
                        break;
                    }

                case 2010:
                    {
                        result = 10;
                        break;
                    }
            }

        giveResult:
            ;
            return result;
        }

        public bool GameStarted
        {
            get
            {
                return _gameStarted;
            }
        }
        /// <summary>
        /// Gets or sets spectator mode state of local player. If set, the client will send request to the server.
        /// </summary>
        public bool IsSpectating
        {
            set
            {
                if (_connected)
                {
                    var specPacket = new[] { (byte)0x3, (byte)0x42, (byte)0x20 };
                    if (value)
                    {
                        specPacket[2] = (byte)(specPacket[2] + 1);
                    }

                    Winsock1SendData(specPacket);
                }
            }

            get
            {
                if (JJ2ClientsSockInfo[socketID] is object)
                {
                    return JJ2ClientsSockInfo[socketID].IsSpectating;
                }
                else
                {
                    return false;
                }
            }
        }

        public bool SendData(byte[] data, JJ2.Network_Protocol protocol)
        {
            if (_connected)
            {
                if ((int)protocol == 0)
                {
                    Winsock1SendData(data);
                }
                else if ((int)protocol == 1)
                {
                    Winsock2SendData(data);
                }
                else
                {
                    return false;
                    return default;
                }

                return true;
            }
            else
            {
                return false;
            }
        }

        public string SendMessage(string msg)
        {
            if (_connected & socketID != 0xFF)
            {
                try
                {
                    var messagePacket = new[] { (byte)0, (byte)0x1B, socketID, (byte)0x20 };
                    msg = Encoding.Default.GetString(messagePacket) + msg;
                    messagePacket = DefaultEncoding.GetBytes(msg);
                    messagePacket[0] = (byte)messagePacket.Length;
                    Winsock1SendData(messagePacket);
                    return Microsoft.VisualBasic.Constants.vbNullString;
                }
                catch (Exception ex)
                {
                    return "Unknown error.";
                }
            }
            else
            {
                return "Client is not connected.";
            }
        }

        public object Whisper(string msg, byte recvPlayer)
        {
            var whispPacketHeader = new[] { (byte)0x0, (byte)0x58, recvPlayer };
            var msgBytes = DefaultEncoding.GetBytes(msg);
            int copyLength = msgBytes.Length;
            if (copyLength > 252)
            {
                copyLength = 252;
            }

            var whispPacket = new byte[(whispPacketHeader.Length + copyLength)];
            Array.Copy(whispPacketHeader, whispPacket, whispPacketHeader.Length);
            Array.Copy(msgBytes, 0, whispPacket, 3, msgBytes.Length);
            whispPacket[0] = (byte)whispPacket.Length;
            Winsock1SendData(whispPacket);
            return default;
        }


        /// <summary>
        /// Gets the list of connected players. Returns NULL if successful.
        public string GetPlayersList(ref byte numOfPlayers, ref string[] playersNames, ref byte[] playersIDs, ref byte[] playersSocketIndexes, ref byte[] playersTeam, ref byte[] playersChar, bool ExcludeMe = false, bool ExcludeHost = false)
        {
            string result = "";
            if (_connected)
            {
                if (_ID != 0xFF & socketID != 0xFF)
                {
                    var playersNamesRes = new List<string>();
                    var playersIDsRes = new List<byte>();
                    var playersSocketIndexesRes = new List<byte>();
                    var playersTeamRes = new List<byte>();
                    var playersCharRes = new List<byte>();
                    numOfPlayers = 0;
                    byte startingFrom;
                    if (ExcludeHost)
                    {
                        startingFrom = 1;
                    }
                    else
                    {
                        startingFrom = 0;
                    }

                    for (byte i = startingFrom; i <= _connectionlimit; i++)
                    {
                        if (JJ2ClientsSockInfo[i] is object)
                        {
                            if ((int)JJ2ClientsSockInfo[i].NumOfPlayers != 0)
                            {
                                foreach (byte anID in JJ2ClientsSockInfo[i].PlayerID)
                                {
                                    if (anID != 0xFF & anID <= _connectionlimit)
                                    {
                                        if (!(anID == _ID & ExcludeMe)) // (Me) NAND (ExcludeMe). This expression will exclude this client if ExcludeMe=TRUE.
                                        {
                                            if (Players[anID] is object)
                                            {
                                                playersSocketIndexesRes.Add(i);
                                                playersIDsRes.Add(anID);
                                                playersNamesRes.Add(Players[(int)anID].Name);
                                                playersTeamRes.Add(Players[(int)anID].Team);
                                                playersCharRes.Add((byte)Players[anID].Character);
                                                numOfPlayers = (byte)(numOfPlayers + 1);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    playersNames = new string[(numOfPlayers + 1)];
                    playersIDs = new byte[(numOfPlayers + 1)];
                    playersSocketIndexes = new byte[(numOfPlayers + 1)];
                    playersTeam = new byte[(numOfPlayers + 1)];
                    playersChar = new byte[(numOfPlayers + 1)];
                    playersSocketIndexes = playersSocketIndexesRes.ToArray();
                    playersIDs = playersIDsRes.ToArray();
                    playersTeam = playersTeamRes.ToArray();
                    playersChar = playersCharRes.ToArray();
                    playersNames = playersNamesRes.ToArray();
                }
                else
                {
                    result = "Connected but not initialized.";
                }
            }
            else
            {
                result = "Not connected.";
            }

            return result;
        }

        public byte GetNumOfSpectators(bool ExcludeHost = false)
        {
            byte result = 0;
            if (_connected)
            {
                if (_ID != 0xFF & socketID != 0xFF)
                {
                    byte startingFrom;
                    if (ExcludeHost)
                    {
                        startingFrom = 1;
                    }
                    else
                    {
                        startingFrom = 0;
                    }

                    for (byte i = startingFrom; i <= _connectionlimit; i++)
                    {
                        if (JJ2ClientsSockInfo[i] is object)
                        {
                            if ((int)JJ2ClientsSockInfo[i].NumOfPlayers != 0)
                            {
                                if (JJ2ClientsSockInfo[i].IsSpectating != false)
                                {
                                    foreach (byte anID in JJ2ClientsSockInfo[i].PlayerID)
                                    {
                                        if (anID != 0xFF & anID <= _connectionlimit)
                                        {
                                            result = (byte)(result + 1);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return result;
        }

        /// <summary>
        /// Gets the state of the connection to server.
        /// </summary>
        public bool Connected
        {
            get
            {
                return _connected;
            }
        }

        /// <summary>
        /// Gets the client's player name assigned by server.
        /// </summary>
        public string CurrentName
        {
            get
            {
                if (_ID < Players.Length)
                {
                    if (Players[_ID] is object)
                    {
                        return Players[_ID].Name;
                        return default;
                    }
                }

                return "";
            }
        }
        /// <summary>
        /// Gets total number of players on the server.
        /// </summary>
        public byte GetNumOfPlayers
        {
            get
            {
                short result = 0;
                if (_connected)
                {
                    foreach (JJ2.JJ2SocketInfo sInf in JJ2ClientsSockInfo)
                    {
                        if (sInf is object)
                        {
                            foreach (byte id in sInf.PlayerID)
                            {
                                if (id != 0xFF)
                                {
                                    result = (short)(result + 1);
                                }
                            }

                            if (result > 255)
                            {
                                result = 255;
                            }
                        }
                    }
                }

                return (byte)result;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns>Number of players on host machine</returns>
        public byte GetNumOfHosts
        {
            get
            {
                short result = 0;
                if (_connected)
                {
                    if (JJ2ClientsSockInfo[0] is object)
                    {
                        foreach (byte anId in JJ2ClientsSockInfo[0].PlayerID)
                        {
                            if (anId != 0xFF)
                            {
                                result = (short)(result + 1);
                            }
                        }

                        if (result > 255)
                        {
                            result = 255;
                        }
                    }
                }

                return (byte)result;
            }
        }
        /// <summary>
        /// Gets current game mode.
        /// </summary>
        public JJ2.JJ2_Game_Type GameType
        {
            get
            {
                return (JJ2.JJ2_Game_Type)_gameType;
            }
        }
        /// <summary>
        /// Gets current custom game mode of JJ2+. Retuens 0 if custom game is disabled.
        /// </summary>
        public JJ2.JJ2_Custom_Game_Type CustomGameMode
        {
            get
            {
                return (JJ2.JJ2_Custom_Game_Type)_customGameMode;
            }
        }
        /// <summary>
        /// Gets current number of scores required to win the game.
        /// </summary>
        public int MaxScore
        {
            get
            {
                return _maxScore;
            }
        }


        /// <summary>
        /// Gets wheather server is a special server (Gem Collecting Challenge).
        /// </summary>
        public bool SpecialServer
        {
            get
            {
                return _specialServer;
            }
        }
        /// <summary>
        /// Gets or sets the client's player name assigned by user (menu).
        /// </summary>
        public string Name
        {
            get
            {
                return _name;
            }

            set
            {
                if (!string.IsNullOrEmpty(value))
                {
                    if (value.Length < 21)
                    {
                        _name = value;
                    }
                }
            }
        }
        /// <summary>
        /// Gets your client ID among the clients in the server.
        /// </summary>
        public byte ClientID
        {
            get
            {
                return socketID;
            }
        }
        /// <summary>
        /// Gets your in-game player ID (Zero-based).
        /// </summary>
        public byte PlayerID
        {
            get
            {
                return _ID;
            }
        }
        /// <summary>
        /// Gets whether you are admin.
        /// </summary>
        public bool IsAdmin
        {
            get
            {
                if (socketID != 0xFF)
                {
                    return JJ2ClientsSockInfo[socketID].IsAdmin;
                }
                else
                {
                    return false;
                }
            }
        }
        /// <summary>
        /// Gets whether spectator mode is ON.
        /// </summary>
        public bool SpectatorMode
        {
            get
            {
                if (socketID != 0xFF)
                {
                    return JJ2ClientsSockInfo[socketID].IsSpectating;
                }
                else
                {
                    return false;
                }
            }
        }

        /// <summary>
        /// Gets whether server is hosted using JJ2+ ot not.
        /// </summary>
        public bool PlusServer
        {
            get
            {
                return _plusServer;
            }
        }

        /// <summary>
        /// Gets server address.
        /// </summary>
        public string RemoteAddress
        {
            get
            {
                if (_connected)
                {
                    return _serverAddress;
                }
                else
                {
                    return "";
                }
            }
        }
        /// <summary>
        /// Gets server IP address.
        /// </summary>
        public string RemoteIP
        {
            get
            {
                if (Connected)
                {
                    return _remoteEP.Address.ToString();
                }
                else
                {
                    return "";
                }
            }
        }
        /// <summary>
        /// Gets server port number.
        /// </summary>
        public ushort RemotePort
        {
            get
            {
                if (Connected)
                {
                    return (ushort)_remoteEP.Port;
                }
                else
                {
                    return 0;
                }
            }
        }
        /// <summary>
        /// Returns 0 if no local UDP port assigned otherwise the return value is the local UDP port number.
        /// </summary>
        public ushort UDPLocalPort
        {
            get
            {
                if (Winsock2 is object)
                {
                    return Conversions.ToUShort(Winsock2.Client.LocalEndPoint.ToString().Split(':')[1]);
                }
                else
                {
                    return 0;
                }
            }
        }
        /// <summary>
        /// Gets the state of end of level screen (black screen, winner is...)
        /// </summary>
        public bool IsLevelCycling
        {
            get
            {
                return _cycling;
            }
        }
        /// <summary>
        /// Gets the name of the current level hosted in remote server.
        /// </summary>
        public string CurrentLevelName
        {
            get
            {
                return _currentLevelName;
            }
        }
        /// <summary>
        /// Gets JJ2+ Game state. returns as JJ2Plus_Game_State.
        /// </summary>
        public JJ2.JJ2Plus_Game_State GetGameState
        {
            get
            {
                return _gameState;
            }
        }
        /// <summary>
        /// Gets a value indicating whether there is ongoing game. This value can be used to determine if players can use "/ready" command.
        /// </summary>
        public bool GameInProgress
        {
            get
            {
                return _gameInProgress;
            }
        }

        /// <summary>
        /// Gets if only JJ2+ users are allowed to join the server.
        /// </summary>
        public bool PlusOnly
        {
            get
            {
                return _plusOnly;
            }
        }
        /// <summary>
        /// Gets the state of scripts.
        /// </summary>
        /// <returns>Returns the state of PlusOnly at level load time.</returns>
        public bool ScriptsEnabled
        {
            get
            {
                return _scriptsEnabled;
            }
        }
    }
}