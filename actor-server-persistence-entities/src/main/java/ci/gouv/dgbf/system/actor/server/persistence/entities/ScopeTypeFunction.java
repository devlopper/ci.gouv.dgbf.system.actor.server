package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeTypeFunction.TABLE_NAME)
public class ScopeTypeFunction extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public ScopeTypeFunction setIdentifier(String identifier) {
		return (ScopeTypeFunction) super.setIdentifier(identifier);
	}
	
	public static final String TABLE_NAME = "TYPE_DOMAINE_FONCTION";	
}