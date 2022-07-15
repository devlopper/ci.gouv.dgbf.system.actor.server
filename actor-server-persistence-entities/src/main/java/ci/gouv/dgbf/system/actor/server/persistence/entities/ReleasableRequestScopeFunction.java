package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.hibernate.annotations.Immutable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ReleasableRequestScopeFunction.TABLE_NAME)
/* Performance Tuning */
@Immutable
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
public class ReleasableRequestScopeFunction extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = "LIBERE_PAR") private String releasedBy;
	
	@Override
	public ReleasableRequestScopeFunction setIdentifier(String identifier) {
		return (ReleasableRequestScopeFunction) super.setIdentifier(identifier);
	}
	
	@Override
	public ReleasableRequestScopeFunction setCode(String code) {
		return (ReleasableRequestScopeFunction) super.setCode(code);
	}
	
	@Override
	public ReleasableRequestScopeFunction setName(String name) {
		return (ReleasableRequestScopeFunction) super.setName(name);
	}
	
	public static final String TABLE_NAME = "VA_POST_A_LIBERE_POUR_DEMAND";	
}