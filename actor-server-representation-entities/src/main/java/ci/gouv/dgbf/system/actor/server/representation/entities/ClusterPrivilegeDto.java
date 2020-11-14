package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ClusterPrivilegeDto extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ClusterPrivilegesDto clusterPrivileges;
	private PrivilegeDto privilege;
	
	@Override
	public ClusterPrivilegeDto setIdentifier(String identifier) {
		return (ClusterPrivilegeDto) super.setIdentifier(identifier);
	}
}