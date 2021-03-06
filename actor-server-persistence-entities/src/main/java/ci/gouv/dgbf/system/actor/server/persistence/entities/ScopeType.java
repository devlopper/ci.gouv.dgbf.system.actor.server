package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeType.TABLE_NAME)
@Cacheable(value = true)
public class ScopeType extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_ORDER_NUMBER) private Byte orderNumber;
	
	@Override
	public ScopeType setIdentifier(String identifier) {
		return (ScopeType) super.setIdentifier(identifier);
	}
	
	@Override
	public ScopeType setCode(String code) {
		return (ScopeType) super.setCode(code);
	}
	
	@Override
	public ScopeType setName(String name) {
		return (ScopeType) super.setName(name);
	}
	
	public static final String FIELD_ORDER_NUMBER = "orderNumber";
	
	public static final String TABLE_NAME = "TYPE_DOMAINE";
	
	public static final String COLUMN_ORDER_NUMBER = "NUMERO_ORDRE";

	public static final String CODE_AB = "AB";
	public static final String CODE_SECTION = "SECTION";
	public static final String CODE_CATEGORIE_ACTIVITE = "CATEGORIE_ACTIVITE";
	
	// Chaine programmatique
	public static final String CODE_USB = "USB";
	public static final String CODE_ACTION = "ACTION";
	public static final String CODE_ACTIVITE = "ACTIVITE";
	public static final String CODE_IMPUTATION = "IMPUTATION";

	// Chaine de gestion
	public static final String CODE_UGP = "UGP";
	public static final String CODE_UA = "UA";
}