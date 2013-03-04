#ifndef PlayerManager_h__
#define PlayerManager_h__

class Player;

typedef std::vector<uint64> vec_playerid;
typedef std::map<uint64, Player*> map_player;
typedef map_player::iterator it_map_player;

class CPlayerManager : public Singleton<CPlayerManager>
{
public:
	CPlayerManager(void);
	~CPlayerManager(void);

public:
	void Run(uint32 elapse);
	void Draw(HDC hdc);

	void SetBlock(int x, int y);
	Player* GetPlayer(uint64 id);
	void AddPlayer(Player *p);
	void RemovePlayer(Player *p);
	Player *NewPlayer() { return m_PlayerAllocator.alloc(); }
	void FreePlayer(Player *p) { m_PlayerAllocator.free(p); }

	void OnMoveTo(uint64 id, int x, int y);
	void ResetPosition(uint64 id, int x, int y);

private:
	FixedSizeAllocator<Player> m_PlayerAllocator;
	map_player m_Player;
};

#define sPlayerMgr CPlayerManager::getSingleton()

#endif // PlayerManager_h__