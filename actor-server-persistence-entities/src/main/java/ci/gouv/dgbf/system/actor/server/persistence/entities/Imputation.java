package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Imputation.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class Imputation extends AbstractImputation implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Imputation setIdentifier(String identifier) {
		return (Imputation) super.setIdentifier(identifier);
	}
	
	@Override
	public Imputation setCode(String code) {
		return (Imputation) super.setCode(code);
	}
	
	@Override
	public Imputation setName(String name) {
		return (Imputation) super.setName(name);
	}
	
	@Override
	public Imputation setActivityFromIdentifier(String identifier) {
		return (Imputation) super.setActivityFromIdentifier(identifier);
	}
	
	public static final String TABLE_NAME = "VM_APP_IMPUTATION";
}