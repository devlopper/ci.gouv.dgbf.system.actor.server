package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=RequestStatus.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class RequestStatus extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public RequestStatus setIdentifier(String identifier) {
		return (RequestStatus) super.setIdentifier(identifier);
	}
	
	@Override
	public RequestStatus setCode(String code) {
		return (RequestStatus) super.setCode(code);
	}
	
	public static final String TABLE_NAME = "DM_STATUT";
	
	public static final String CODE_INITIALIZED = "INITIEE";
	public static final String CODE_SUBMITTED = "SOUMISE";
	public static final String CODE_ACCEPTED = "ACCEPTEE";
	public static final String CODE_REJECTED = "REJETEE";
	
	public static final String LABEL = "Statut";
	
	public static final Collection<String> CODES_ACCEPTED_REJECTED = List.of(RequestStatus.CODE_ACCEPTED,RequestStatus.CODE_REJECTED);
}