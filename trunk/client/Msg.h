#ifndef Msg_h__
#define Msg_h__

#pragma pack(push)
#pragma pack(1)
struct stMsgHead
{
	stMsgHead(uint16 _len = 5, uint8 _iszip = 0, uint16 _cmd = 0)
	{
		len = htons(_len);
		iszip = _iszip;
		cmd = htons(_cmd);
	}
	uint16 len;
	uint8 iszip;
	uint16 cmd;
};
#pragma pack(pop)

#endif // Msg_h__
