package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=RequestFunction.TABLE_NAME)
public class RequestFunction extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_REQUEST) @NotNull private Request request;
	@ManyToOne @JoinColumn(name = COLUMN_FUNCTION) @NotNull private Function function;
	
	@Override
	public RequestFunction setIdentifier(String identifier) {
		return (RequestFunction) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_REQUEST = "request";
	public static final String FIELD_FUNCTION = "function";
	
	public static final String TABLE_NAME = "DM_FONCTION";
	
	public static final String COLUMN_REQUEST = "DEMANDE";
	public static final String COLUMN_FUNCTION = "FONCTION";
}