package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ClusterPrivileges.TABLE_NAME)
public class ClusterPrivileges extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name=COLUMN_CLUSTER) private Cluster cluster;
	
	@Override
	public ClusterPrivileges setIdentifier(String identifier) {
		return (ClusterPrivileges) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_CLUSTER = "cluster";
	
	public static final String TABLE_NAME = "DA_HABILITATION";
	
	public static final String COLUMN_CLUSTER = "DOMAINE";
}