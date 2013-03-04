#ifndef client_h__
#define client_h__

class Player;

class Client : public Singleton<Client>
{
public:
	Client(HINSTANCE hInstance);
	~Client();

public:
	void Initialize(int iCmdShow);
	int Run();
	LRESULT HandleEvent(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam); 	

	void LoadContent();
	void UnLoadContent();
	void Update(uint32 elapse); 
	void Draw(HWND hwnd);
	HWND GetWnd() const { return m_hwnd; }

	void MouseMove(LPARAM lParam);
	void MouseLDButtonDown(LPARAM lParam);
	void MouseLButtonDown(LPARAM lParam);
	void MouseRButtonDown(LPARAM lParam);
	void MouseMButtonDown(LPARAM lParam);
	void KeyDown(LPARAM wParam);
	
private:
	HINSTANCE m_hInstance;
	HWND m_hwnd;
	int m_nMouseX;
	int m_nMouseY;
};

#define sClient Client::getSingleton()

#endif // client_h__