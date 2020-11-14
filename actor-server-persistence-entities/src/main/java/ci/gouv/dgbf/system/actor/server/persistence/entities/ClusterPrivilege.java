package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ClusterPrivilege.TABLE_NAME)
public class ClusterPrivilege extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name=COLUMN_CLUSTER_PRIVILEGES) private ClusterPrivileges clusterPrivileges;
	@ManyToOne @JoinColumn(name=COLUMN_PRIVILEGE) private Privilege privilege;
	
	@Override
	public ClusterPrivilege setIdentifier(String identifier) {
		return (ClusterPrivilege) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_CLUSTER_PRIVILEGES = "clusterPrivileges";
	public static final String FIELD_PRIVILEGE = "privilege";
	
	public static final String TABLE_NAME = "DA_PRIVILEGE";
	
	public static final String COLUMN_CLUSTER_PRIVILEGES = "HABILITATION";
	public static final String COLUMN_PRIVILEGE = "PRIVILEGE";
}