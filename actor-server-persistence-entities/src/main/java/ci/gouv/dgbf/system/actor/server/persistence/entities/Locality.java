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
@Entity @Table(name=Locality.TABLE_NAME)
public class Locality extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Locality setIdentifier(String identifier) {
		return (Locality) super.setIdentifier(identifier);
	}
	
	@Override
	public Locality setCode(String code) {
		return (Locality) super.setCode(code);
	}
	
	@Override
	public Locality setName(String name) {
		return (Locality) super.setName(name);
	}
	
	public static final String TABLE_NAME = "VM_APP_LOCALITE";
	
	public static final String CODE_SOUS_PREFECTURE_BINGERVILLE = "780102";
}