package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=RequestType.TABLE_NAME)
public class RequestType extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_FORM) @NotNull private IdentificationForm form;
	
	@Transient private String formAsString;
	
	@Override
	public RequestType setIdentifier(String identifier) {
		return (RequestType) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_FORM = "form";
	public static final String FIELD_FORM_AS_STRING = "formAsString";
	
	public static final String TABLE_NAME = "DM_TYPE";
	
	public static final String COLUMN_FORM = "FORMULAIRE";
}