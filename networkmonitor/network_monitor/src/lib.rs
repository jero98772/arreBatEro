use pnet::datalink::{self, NetworkInterface, Channel::Ethernet};
use pnet::packet::{Packet, ipv4::Ipv4Packet};
use pnet::packet::ethernet::{EtherTypes, EthernetPacket};
use pyo3::prelude::*;

fn handle_packet(packet: &[u8]) -> Option<String> {
    if let Some(ethernet_packet) = EthernetPacket::new(packet) {
        match ethernet_packet.get_ethertype() {
            EtherTypes::Ipv4 => {
                if let Some(ipv4_packet) = Ipv4Packet::new(ethernet_packet.payload()) {
                    return Some(format!("Captured IPv4 packet: {:?}", ipv4_packet));
                }
            },
            _ => {}
        }
    }
    None
}

#[pyfunction]
pub fn sniff(interface_name: &str) -> PyResult<Vec<String>> {
    let interfaces = datalink::interfaces();
    let interface = interfaces.into_iter()
        .filter(|iface: &NetworkInterface| iface.name == interface_name)
        .next()
        .expect("Error getting network interface");

    let mut packets = Vec::new();

    match datalink::channel(&interface, Default::default()) {
        Ok(Ethernet(_, mut rx)) => {
            for _ in 0..10 {
                match rx.next() {
                    Ok(packet) => {
                        if let Some(packet_info) = handle_packet(packet) {
                            packets.push(packet_info);
                        }
                    },
                    Err(e) => eprintln!("An error occurred while reading: {}", e),
                }
            }
        },
        Ok(_) => eprintln!("Unhandled channel type"),
        Err(e) => eprintln!("An error occurred while creating datalink channel: {}", e),
    }

    Ok(packets)
}

#[pymodule]
fn network_monitor(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(sniff, m)?)?;
    Ok(())
}
