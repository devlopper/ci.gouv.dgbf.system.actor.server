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
@Entity @Table(name=ClusterAdministrator.TABLE_NAME)
public class ClusterAdministrator extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name=COLUMN_CLUSTER) private Cluster cluster;
	@ManyToOne @JoinColumn(name=COLUMN_ACTOR) private Actor actor;
	
	@Override
	public ClusterAdministrator setIdentifier(String identifier) {
		return (ClusterAdministrator) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_CLUSTER = "cluster";
	public static final String FIELD_ACTOR = "actor";
	
	public static final String TABLE_NAME = "DA_ADMINISTRATEUR";
	
	public static final String COLUMN_CLUSTER = "DOMAINE";
	public static final String COLUMN_ACTOR = "ADMINISTRATEUR";
}