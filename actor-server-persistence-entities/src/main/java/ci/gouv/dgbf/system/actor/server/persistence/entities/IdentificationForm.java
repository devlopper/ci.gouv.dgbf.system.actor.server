package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.Collection;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=IdentificationForm.TABLE_NAME)
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class IdentificationForm extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Transient private Collection<IdentificationAttribute> attributs;
	
	@Override
	public IdentificationForm setIdentifier(String identifier) {
		return (IdentificationForm) super.setIdentifier(identifier);
	}
	
	@Override
	public IdentificationForm setCode(String code) {
		return (IdentificationForm) super.setCode(code);
	}
	
	public static final String TABLE_NAME = "ID_FORMULAIRE";
	
	public static final String CODE_CREATION_COMPTE = "CREATION_COMPTE";
}