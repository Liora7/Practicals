//
// Generated file, do not edit! Created by nedtool 5.6 from MessagePacket.msg.
//

#ifndef __MESSAGEPACKET_M_H
#define __MESSAGEPACKET_M_H

#if defined(__clang__)
#  pragma clang diagnostic ignored "-Wreserved-id-macro"
#endif
#include <omnetpp.h>

// nedtool version check
#define MSGC_VERSION 0x0506
#if (MSGC_VERSION!=OMNETPP_VERSION)
#    error Version mismatch! Probably this file was generated by an earlier version of nedtool: 'make clean' should help.
#endif



/**
 * Class generated from <tt>MessagePacket.msg:1</tt> by nedtool.
 * <pre>
 * packet MessagePacket
 * {
 *     string source;
 *     string destination;
 *     string data;
 * }
 * </pre>
 */
class MessagePacket : public ::omnetpp::cPacket
{
  protected:
    ::omnetpp::opp_string source;
    ::omnetpp::opp_string destination;
    ::omnetpp::opp_string data;

  private:
    void copy(const MessagePacket& other);

  protected:
    // protected and unimplemented operator==(), to prevent accidental usage
    bool operator==(const MessagePacket&);

  public:
    MessagePacket(const char *name=nullptr, short kind=0);
    MessagePacket(const MessagePacket& other);
    virtual ~MessagePacket();
    MessagePacket& operator=(const MessagePacket& other);
    virtual MessagePacket *dup() const override {return new MessagePacket(*this);}
    virtual void parsimPack(omnetpp::cCommBuffer *b) const override;
    virtual void parsimUnpack(omnetpp::cCommBuffer *b) override;

    // field getter/setter methods
    virtual const char * getSource() const;
    virtual void setSource(const char * source);
    virtual const char * getDestination() const;
    virtual void setDestination(const char * destination);
    virtual const char * getData() const;
    virtual void setData(const char * data);
};

inline void doParsimPacking(omnetpp::cCommBuffer *b, const MessagePacket& obj) {obj.parsimPack(b);}
inline void doParsimUnpacking(omnetpp::cCommBuffer *b, MessagePacket& obj) {obj.parsimUnpack(b);}


#endif // ifndef __MESSAGEPACKET_M_H

