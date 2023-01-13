package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.regex.Pattern;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Country.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class Country extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Country setIdentifier(String identifier) {
		return (Country) super.setIdentifier(identifier);
	}
	
	@Override
	public Country setCode(String code) {
		return (Country) super.setCode(code);
	}
	
	@Override
	public Country setName(String name) {
		return (Country) super.setName(name);
	}
	
	@Override
	public String toString() {
		return name;
	}
	
	public static final String TABLE_NAME = "VM_APP_PAYS";
	
	public static final String CODE_COTE_IVOIRE = "0000";
	public static final Pattern PHONE_NUMBER_PATTERN_COTE_IVOIRE = Pattern.compile("^\\d{10}$");
	
	public static final String LABEL = "Pays";
}