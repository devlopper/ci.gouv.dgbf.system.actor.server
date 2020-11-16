package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=IdentificationFormAttribut.TABLE_NAME)
public class IdentificationFormAttribut extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_FORM) private IdentificationForm form;
	@ManyToOne @JoinColumn(name = COLUMN_ATTRIBUT) private IdentificationAttribut attribut;
	@Column(name = COLUMN_ORDER_NUMBER) private Integer orderNumber;
	@Column(name = COLUMN_REQUIRED) private Boolean required;
	
	@Override
	public IdentificationFormAttribut setIdentifier(String identifier) {
		return (IdentificationFormAttribut) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_FORM = "form";
	public static final String FIELD_ATTRIBUT = "attribut";
	public static final String FIELD_ORDER_NUMBER = "orderNumber";
	public static final String FIELD_REQUIRED = "required";
	
	public static final String TABLE_NAME = "ID_CHAMP";
	
	public static final String COLUMN_FORM = "FORMULAIRE";
	public static final String COLUMN_ATTRIBUT = "ATTRIBUT";
	public static final String COLUMN_ORDER_NUMBER = "NUMERO_ORDRE";
	public static final String COLUMN_REQUIRED = "OBLIGATOIRE";
}