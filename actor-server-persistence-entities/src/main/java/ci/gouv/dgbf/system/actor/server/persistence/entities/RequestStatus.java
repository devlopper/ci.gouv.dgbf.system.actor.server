package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=RequestStatus.TABLE_NAME)
public class RequestStatus extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public RequestStatus setIdentifier(String identifier) {
		return (RequestStatus) super.setIdentifier(identifier);
	}
	
	public static final String TABLE_NAME = "DM_STATUT";
	
	public static final String CODE_INITIALIZED = "INITIEE";
	public static final String CODE_ACCEPTED = "ACCEPTEE";
	public static final String CODE_REJECTED = "REJETEE";
}